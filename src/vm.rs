use arrayvec::ArrayVec;
use rustc_hash::FxHashMap;
use std::time::{self, SystemTime};

use crate::{
    cast,
    chunk::{Chunk, Op, Value},
    compiler::Parser,
    error::LoxError,
    memory::{HeapId, Memory},
    object::{Closure, NativeFunction, ObjData, Upvalue},
};

pub struct Vm {
    frames: ArrayVec<CallFrame, { Vm::FRAMES_MAX }>,
    stack: ArrayVec<Value, { Vm::STACK_MAX }>,
    memory: Memory,
    globals: FxHashMap<HeapId, Value>,
    open_upvalue: Option<HeapId>,
}

impl Vm {
    const FRAMES_MAX: usize = 64;
    const STACK_MAX: usize = Vm::FRAMES_MAX * (u8::MAX as usize + 1);

    pub fn new(memory: Memory) -> Self {
        let mut vm = Self {
            frames: ArrayVec::new(),
            stack: ArrayVec::new(),
            memory,
            globals: FxHashMap::default(),
            open_upvalue: None,
        };

        vm.define_native("clock", NativeFunction(clock_native));
        vm
    }

    pub fn interpret(&mut self, code: &str) -> Result<(), LoxError> {
        let function = Parser::new(&mut self.memory, code).compile()?;

        let fun_id = self.memory.alloc(ObjData::Function(function));
        self.stack.push(Value::Function(fun_id));

        let closure = Closure::new(fun_id);
        let closure_id = self.memory.alloc(ObjData::Closure(closure));
        self.call(closure_id, 0).and_then(|_| self.run())
    }

    pub fn run(&mut self) -> Result<(), LoxError> {
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
                            let str_a = cast!(self.memory.lookup(a), ObjData::String);
                            let str_b = cast!(self.memory.lookup(b), ObjData::String);
                            let result = str_a.to_owned() + str_b;
                            let result = self.memory.intern(&result);
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
                    let value = self.stack[slot];
                    self.push(value);
                }
                Op::SetLocal(index) => {
                    let slot = self.current_frame().slot + index as usize;
                    self.stack[slot] = self.peek(0);
                }
                Op::GetGlobal(index) => {
                    let str_id = self.current_chunk().read_string(index);
                    match self.globals.get(&str_id) {
                        Some(&value) => self.push(value),
                        None => {
                            let name = cast!(self.memory.lookup(str_id), ObjData::String);
                            let msg = format!("Undefined variable '{}'.", name);
                            return Err(self.runtime_error(&msg));
                        }
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
                        let name = cast!(self.memory.lookup(str_id), ObjData::String);
                        let msg = format!("Undefined variable '{}'.", name);
                        return Err(self.runtime_error(&msg));
                    }
                }
                Op::GetUpvalue(slot) => {
                    let value = {
                        let current_closure = self.current_closure();
                        let upvalue_id = current_closure.upvalues[slot as usize];
                        let upvalue = cast!(self.memory.lookup(upvalue_id), ObjData::Upvalue);

                        match upvalue.closed {
                            Some(value) => value,
                            None => self.stack[upvalue.location],
                        }
                    };

                    self.push(value);
                }
                Op::SetUpvalue(slot) => {
                    let value = self.peek(0);
                    let upvalue_id = self.current_closure().upvalues[slot as usize];
                    let mut upvalue = cast!(self.memory.lookup_mut(upvalue_id), ObjData::Upvalue);

                    if upvalue.closed.is_none() {
                        self.stack[upvalue.location] = value;
                    } else {
                        upvalue.closed = Some(value);
                    }
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
                Op::Negate => match self.peek(0) {
                    Value::Number(value) => {
                        self.pop();
                        self.push(Value::Number(-value));
                    }
                    _ => return Err(self.runtime_error("Operand must be a number.")),
                },
                Op::Print => {
                    // TODO: should use display trait somehow
                    match self.pop() {
                        Value::Bool(value) => println!("{}", value),
                        Value::Nil => println!("nil"),
                        Value::Number(value) => println!("{}", value),
                        Value::String(str_id) => {
                            println!("{}", cast!(self.memory.lookup(str_id), ObjData::String))
                        }
                        Value::Function(fun_id) => {
                            let fn_name = cast!(self.memory.lookup(fun_id), ObjData::Function).name;
                            match fn_name {
                                Some(str_id) => {
                                    println!(
                                        "<fn {}>",
                                        cast!(self.memory.lookup(str_id), ObjData::String)
                                    );
                                }
                                None => panic!("Expected function name"),
                            }
                        }
                        Value::NativeFunction(_) => println!("<native fn>"),
                        Value::Closure(closure_id) => {
                            // TODO: duplicated
                            let fun_id =
                                cast!(self.memory.lookup(closure_id), ObjData::Closure).fun_id;
                            let fn_name = cast!(self.memory.lookup(fun_id), ObjData::Function).name;
                            match fn_name {
                                Some(str_id) => {
                                    println!(
                                        "<fn {}>",
                                        cast!(self.memory.lookup(str_id), ObjData::String)
                                    );
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
                        // TODO: double lookup?
                        let upvalues = cast!(self.memory.lookup(fun_id), ObjData::Function)
                            .upvalues
                            .len();
                        let mut closure = Closure::new(fun_id);

                        (0..upvalues).for_each(|upvalue| {
                            let upvalue = &cast!(self.memory.lookup(fun_id), ObjData::Function)
                                .upvalues[upvalue];
                            let obj_upvalue = if upvalue.is_local {
                                let location = self.current_frame().slot + upvalue.index as usize;
                                self.capture_upvalue(location)
                            } else {
                                self.current_closure().upvalues[upvalue.index as usize]
                            };

                            closure.upvalues.push(obj_upvalue);
                        });

                        let closure_id = self.memory.alloc(ObjData::Closure(closure));
                        self.push(Value::Closure(closure_id));
                    } else {
                        panic!("Closure should wrap a function");
                    }
                }
                Op::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.pop();
                }
                Op::Return => {
                    let frame = self.frames.pop().unwrap();
                    self.close_upvalues(frame.slot);

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

    pub fn binary_op<T>(
        &mut self,
        op: fn(f64, f64) -> T,
        to_val: fn(T) -> Value,
    ) -> Result<(), LoxError> {
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
        self.stack[size - 1 - distance]
    }

    fn runtime_error(&self, message: &str) -> LoxError {
        eprintln!("{}", message);

        self.frames.iter().rev().for_each(|frame| {
            let closure = cast!(self.memory.lookup(frame.closure_id), ObjData::Closure);
            let function = cast!(self.memory.lookup(closure.fun_id), ObjData::Function);
            let line = frame.ip - 1;

            match function.name {
                Some(str_id) => {
                    let function_name = cast!(self.memory.lookup(str_id), ObjData::String);
                    eprintln!(
                        "[line {}] in {}()",
                        function.chunk.lines[line], function_name
                    );
                }
                None => eprintln!("[line {}] in script", function.chunk.lines[line]),
            };
        });

        LoxError::RuntimeError
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn current_closure(&self) -> &Closure {
        let closure_id = self.current_frame().closure_id;
        cast!(self.memory.lookup(closure_id), ObjData::Closure)
    }

    fn current_chunk(&self) -> &Chunk {
        let closure = self.current_closure();
        let function = cast!(self.memory.lookup(closure.fun_id), ObjData::Function);
        &function.chunk
    }

    fn call_value(&mut self, arg_count: usize) -> Result<(), LoxError> {
        match self.peek(arg_count) {
            Value::Closure(closure_id) => self.call(closure_id, arg_count),
            Value::NativeFunction(native) => {
                let left = self.stack.len() - arg_count;
                let result = native.0(&self.stack[left..]);
                self.push(result);
                Ok(())
            }
            _ => Err(self.runtime_error("Can only call functions and classes.")),
        }
    }

    fn call(&mut self, closure_id: HeapId, arg_count: usize) -> Result<(), LoxError> {
        let closure = cast!(self.memory.lookup(closure_id), ObjData::Closure);
        let function = cast!(self.memory.lookup(closure.fun_id), ObjData::Function);

        if arg_count != function.arity {
            let message = format!(
                "Expected {} arguments but got {}.",
                function.arity, arg_count
            );

            Err(self.runtime_error(&message))
        } else if self.frames.len() == Vm::FRAMES_MAX {
            Err(self.runtime_error("Stack overflow."))
        } else {
            let frame = CallFrame::new(closure_id, self.stack.len() - arg_count - 1);
            self.frames.push(frame);

            Ok(())
        }
    }

    fn define_native(&mut self, name: &str, native: NativeFunction) {
        let name = self.memory.intern(name);
        self.globals.insert(name, Value::NativeFunction(native));
    }

    fn capture_upvalue(&mut self, location: usize) -> HeapId {
        let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalue;

        // TODO: double lookup
        while let Some(inner) = upvalue.filter(|upvalue| {
            cast!(self.memory.lookup(*upvalue), ObjData::Upvalue).location > location
        }) {
            upvalue = cast!(self.memory.lookup(inner), ObjData::Upvalue).next;
            prev_upvalue = Some(inner);
        }

        if let Some(inner) = upvalue.filter(|upvalue| {
            cast!(self.memory.lookup(*upvalue), ObjData::Upvalue).location == location
        }) {
            return inner;
        }

        let mut created_upvalue = Upvalue::new(location);
        created_upvalue.next = upvalue;
        let upvalue_id = self.memory.alloc(ObjData::Upvalue(created_upvalue));

        if let Some(inner) = prev_upvalue {
            cast!(self.memory.lookup_mut(inner), ObjData::Upvalue).next = Some(upvalue_id);
        } else {
            self.open_upvalue = Some(upvalue_id);
        }

        upvalue_id
    }

    fn close_upvalues(&mut self, last: usize) {
        while let Some(upvalue) = self.open_upvalue.filter(|upvalue| {
            cast!(self.memory.lookup(*upvalue), ObjData::Upvalue).location >= last
        }) {
            let mut upvalue = cast!(self.memory.lookup_mut(upvalue), ObjData::Upvalue);
            let value = self.stack[upvalue.location];
            upvalue.closed = Some(value);
            self.open_upvalue = upvalue.next.take();
        }
    }
}

#[derive(Debug)]
struct CallFrame {
    closure_id: HeapId,
    ip: usize,
    slot: usize,
}

impl CallFrame {
    fn new(closure_id: HeapId, slot: usize) -> Self {
        Self {
            closure_id,
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
