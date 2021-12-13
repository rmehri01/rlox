use arrayvec::ArrayVec;
use rustc_hash::FxHashMap;
use std::time::{self, SystemTime};

use crate::{
    chunk::{Chunk, Op, Value},
    compiler::Parser,
    error::{LoxError, RuntimeError, RuntimeResult},
    interner::{Interner, StrId},
    object::{Closure, Functions, NativeFunction, Upvalue},
};

pub struct Vm<'intern> {
    frames: ArrayVec<CallFrame, { Vm::FRAMES_MAX }>,
    stack: ArrayVec<Value, { Vm::STACK_MAX }>,
    interner: Interner<'intern>,
    functions: Functions,
    globals: FxHashMap<StrId, Value>,
}

impl<'intern, 'code> Vm<'intern> {
    const FRAMES_MAX: usize = 64;
    const STACK_MAX: usize = Vm::FRAMES_MAX * (u8::MAX as usize + 1);

    pub fn new(interner: Interner<'intern>) -> Self {
        let mut vm = Self {
            frames: ArrayVec::new(),
            stack: ArrayVec::new(),
            interner,
            functions: Functions::new(),
            globals: FxHashMap::default(),
        };

        vm.define_native("clock", NativeFunction(clock_native));
        vm
    }

    pub fn interpret(&mut self, code: &'code str) -> Result<(), LoxError> {
        let function = Parser::new(&mut self.interner, &mut self.functions, code).compile()?;

        let fun_id = self.functions.add(function);
        self.stack.push(Value::Function(fun_id));

        let closure = Closure::new(fun_id);
        self.call(closure, 0)
            .and_then(|_| self.run())
            .map_err(|_| LoxError::Runtime)
    }

    pub fn run(&mut self) -> RuntimeResult {
        loop {
            match self.read_op() {
                Op::Constant(index) => {
                    let constant = self.current_chunk().read_constant(index);
                    self.push(constant);
                }
                Op::Add => {
                    let (b, a) = (self.pop(), self.pop());
                    match (a, b) {
                        (Value::String(a), Value::String(b)) => {
                            let str_a = self.interner.lookup(a);
                            let str_b = self.interner.lookup(b);
                            let result = str_a.to_owned() + str_b;
                            let result = self.interner.intern(&result);
                            self.push(Value::String(result));
                        }
                        (Value::Number(a), Value::Number(b)) => self.push(Value::Number(a + b)),
                        _ => {
                            return Err(
                                self.runtime_error("Operands must be two numbers or two strings.")
                            );
                        }
                    };
                }
                Op::Nil => self.push(Value::Nil),
                Op::True => self.push(Value::Bool(true)),
                Op::False => self.push(Value::Bool(false)),
                Op::Pop => {
                    self.pop();
                }
                Op::GetLocal(index) => {
                    let slot = self.current_frame().slot + index as usize;
                    let value = self.stack[slot].clone();
                    self.push(value);
                }
                Op::SetLocal(index) => {
                    let slot = self.current_frame().slot + index as usize;
                    self.stack[slot] = self.peek(0);
                }
                Op::GetGlobal(index) => {
                    let str_id = self.current_chunk().read_string(index);
                    if let Some(value) = self.globals.get(&str_id) {
                        let value = value.clone();
                        self.push(value);
                    } else {
                        let name = self.interner.lookup(str_id);
                        let msg = format!("Undefined variable '{}'.", name);
                        return Err(self.runtime_error(&msg));
                    }
                }
                Op::DefineGlobal(index) => {
                    let str_id = self.current_chunk().read_string(index);
                    let name = self.pop();
                    self.globals.insert(str_id, name);
                }
                Op::SetGlobal(index) => {
                    let str_id = self.current_chunk().read_string(index);
                    let value = self.peek(0);
                    if self.globals.insert(str_id, value).is_none() {
                        self.globals.remove(&str_id);
                        let name = self.interner.lookup(str_id);
                        let msg = format!("Undefined variable '{}'.", name);
                        return Err(self.runtime_error(&msg));
                    }
                }
                Op::GetUpvalue(slot) => self.push(
                    self.stack[self.current_closure().upvalues[slot as usize].location].clone(),
                ),
                Op::SetUpvalue(slot) => {
                    let location = self.current_closure().upvalues[slot as usize].location;
                    self.stack[location] = self.peek(0);
                }
                Op::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a == b));
                }
                Op::Greater => self.binary_op(|a, b| a > b, Value::Bool)?,
                Op::Less => self.binary_op(|a, b| a < b, Value::Bool)?,
                Op::Subtract => self.binary_op(|a, b| a - b, Value::Number)?,
                Op::Multiply => self.binary_op(|a, b| a * b, Value::Number)?,
                Op::Divide => self.binary_op(|a, b| a / b, Value::Number)?,
                Op::Not => {
                    let value = self.pop();
                    self.push(Value::Bool(value.is_falsey()));
                }
                Op::Negate => {
                    if let Value::Number(value) = self.peek(0) {
                        self.pop();
                        self.push(Value::Number(-value));
                    } else {
                        return Err(self.runtime_error("Operand must be a number."));
                    }
                }
                Op::Print => {
                    // TODO: should use display trait somehow
                    match self.pop() {
                        Value::Bool(value) => println!("{}", value),
                        Value::Nil => println!("nil"),
                        Value::Number(value) => println!("{}", value),
                        Value::String(str_id) => println!("{}", self.interner.lookup(str_id)),
                        Value::Function(fun_id) => {
                            let fn_name = self.functions.lookup(fun_id).name;
                            match fn_name {
                                Some(str_id) => {
                                    println!("<fn {}>", self.interner.lookup(str_id));
                                }
                                None => panic!("Expected function name"),
                            }
                        }
                        Value::NativeFunction(_) => println!("<native fn>"),
                        Value::Closure(Closure { fun_id, .. }) => {
                            // TODO: duplicated
                            let fn_name = self.functions.lookup(fun_id).name;
                            match fn_name {
                                Some(str_id) => {
                                    println!("<fn {}>", self.interner.lookup(str_id));
                                }
                                None => panic!("Expected function name"),
                            }
                        }
                    };
                }
                Op::Jump(offset) => {
                    self.current_frame_mut().ip += offset as usize;
                }
                Op::JumpIfFalse(offset) => {
                    if self.peek(0).is_falsey() {
                        self.current_frame_mut().ip += offset as usize;
                    }
                }
                Op::Loop(offset) => {
                    self.current_frame_mut().ip -= offset as usize;
                }
                Op::Call(arg_count) => {
                    self.call_value(arg_count as usize)?;
                }
                Op::Closure(index) => {
                    let constant = self.current_chunk().read_constant(index);

                    if let Value::Function(fun_id) = constant {
                        let upvalues = &self.functions.lookup(fun_id).upvalues;
                        let mut closure = Closure::new(fun_id);

                        upvalues.iter().for_each(|upvalue| {
                            let obj_upvalue = if upvalue.is_local {
                                let location = self.current_frame().slot + upvalue.index as usize;
                                self.capture_upvalue(location)
                            } else {
                                self.current_closure().upvalues[upvalue.index as usize].clone()
                            };

                            closure.upvalues.push(obj_upvalue);
                        });

                        self.push(Value::Closure(closure));
                    } else {
                        panic!("Closure should wrap a function");
                    }
                }
                Op::Return => {
                    let frame = self.frames.pop().unwrap();
                    let result = self.pop();

                    if self.frames.is_empty() {
                        return Ok(());
                    }

                    self.stack.truncate(frame.slot);
                    self.push(result);
                }
            }
        }
    }

    pub fn binary_op<T>(&mut self, op: fn(f64, f64) -> T, to_val: fn(T) -> Value) -> RuntimeResult {
        let b = self.pop();
        let a = self.pop();
        match (b, a) {
            (Value::Number(b_val), Value::Number(a_val)) => {
                self.push(to_val(op(a_val, b_val)));
                Ok(())
            }
            _ => Err(self.runtime_error("Operands must be numbers.")),
        }
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("empty stack")
    }

    fn read_op(&mut self) -> Op {
        let frame = self.current_frame();
        let op = self.current_chunk().read(frame.ip);
        self.current_frame_mut().ip += 1;
        op
    }

    fn peek(&self, distance: usize) -> Value {
        let size = self.stack.len();
        self.stack[size - 1 - distance].clone()
    }

    fn runtime_error(&self, message: &str) -> RuntimeError {
        eprintln!("{}", message);

        self.frames.iter().rev().for_each(|frame| {
            let function = self.functions.lookup(frame.closure.fun_id);
            let line = frame.ip - 1;

            match function.name {
                Some(str_id) => {
                    let function_name = self.interner.lookup(str_id);
                    eprintln!(
                        "[line {}] in {}()",
                        function.chunk.lines[line], function_name
                    );
                }
                None => eprintln!("[line {}] in script", function.chunk.lines[line]),
            };
        });

        RuntimeError
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn current_closure(&self) -> &Closure {
        &self.current_frame().closure
    }

    fn current_chunk(&self) -> &Chunk {
        let closure = self.current_closure();
        let function = self.functions.lookup(closure.fun_id);
        &function.chunk
    }

    fn call_value(&mut self, arg_count: usize) -> RuntimeResult {
        match self.peek(arg_count) {
            Value::Closure(closure) => self.call(closure, arg_count),
            Value::NativeFunction(native) => {
                let left = self.stack.len() - arg_count;
                let result = native.0(&self.stack[left..]);
                self.push(result);
                Ok(())
            }
            _ => Err(self.runtime_error("Can only call functions and classes.")),
        }
    }

    fn call(&mut self, closure: Closure, arg_count: usize) -> RuntimeResult {
        let function = self.functions.lookup(closure.fun_id);

        if arg_count != function.arity {
            let message = format!(
                "Expected {} arguments but got {}.",
                function.arity, arg_count
            );

            Err(self.runtime_error(&message))
        } else if self.frames.len() == Vm::FRAMES_MAX {
            Err(self.runtime_error("Stack overflow."))
        } else {
            let frame = CallFrame::new(closure, self.stack.len() - arg_count - 1);
            self.frames.push(frame);

            Ok(())
        }
    }

    fn define_native(&mut self, name: &str, native: NativeFunction) {
        let name = self.interner.intern(name);
        self.globals.insert(name, Value::NativeFunction(native));
    }

    fn capture_upvalue(&self, location: usize) -> Upvalue {
        Upvalue::new(location)
    }
}

#[derive(Debug)]
struct CallFrame {
    closure: Closure,
    ip: usize,
    slot: usize,
}

impl CallFrame {
    fn new(closure: Closure, slot: usize) -> Self {
        Self {
            closure,
            ip: 0,
            slot,
        }
    }
}

fn clock_native(_args: &[Value]) -> Value {
    let start = SystemTime::now();
    let time = start
        .duration_since(time::UNIX_EPOCH)
        .expect("Time went backwards");
    Value::Number(time.as_secs_f64())
}
