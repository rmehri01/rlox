use std::time::Instant;

use arrayvec::ArrayVec;
use rustc_hash::FxHashMap;

use crate::{
    chunk::{Chunk, Op, Value},
    compiler::Parser,
    error::LoxError,
    memory::{HeapId, Memory},
    object::{BoundMethod, Class, Closure, Instance, NativeFunction, ObjData, Upvalue},
};

pub struct Vm {
    frames: ArrayVec<CallFrame, { Vm::FRAMES_MAX }>,
    stack: ArrayVec<Value, { Vm::STACK_MAX }>,
    memory: Memory,
    globals: FxHashMap<HeapId, Value>,
    open_upvalue: Option<HeapId>,
    init_string: HeapId,
    start_time: Option<Instant>,
}

impl Vm {
    const FRAMES_MAX: usize = 64;
    const STACK_MAX: usize = Self::FRAMES_MAX * (u8::MAX as usize + 1);

    pub fn new() -> Self {
        let mut memory = Memory::new();
        let init_string = memory.intern("init");

        let mut vm = Self {
            frames: ArrayVec::new(),
            stack: ArrayVec::new(),
            memory,
            globals: FxHashMap::default(),
            open_upvalue: None,
            init_string,
            start_time: None,
        };

        vm.define_native("clock", NativeFunction(clock_native));
        vm
    }

    pub fn interpret(&mut self, code: &str) -> Result<(), LoxError> {
        self.start_time = Some(Instant::now());

        let function = Parser::new(&mut self.memory, code).compile()?;

        let fun_id = self.memory.alloc(ObjData::Function(function));
        self.stack.push(Value::Function(fun_id));

        let closure = Closure::new(fun_id);
        let closure_id = self.alloc(ObjData::Closure(closure));
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
                            let str_a = self.memory.deref(a).as_string().unwrap();
                            let str_b = self.memory.deref(b).as_string().unwrap();
                            let result = str_a.clone() + str_b;
                            let result = self.intern(&result);
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
                            let name = self.memory.deref(str_id).as_string().unwrap();
                            let message = format!("Undefined variable '{}'.", name);
                            return Err(self.runtime_error(&message));
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
                        let name = self.memory.deref(str_id).as_string().unwrap();
                        let message = format!("Undefined variable '{}'.", name);
                        return Err(self.runtime_error(&message));
                    }
                }
                Op::GetUpvalue(slot) => {
                    let value = {
                        let current_closure = self.current_closure();
                        let upvalue_id = current_closure.upvalues[slot as usize];
                        let upvalue = self.memory.deref(upvalue_id).as_upvalue().unwrap();

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
                    let mut upvalue = self.memory.deref_mut(upvalue_id).as_upvalue_mut().unwrap();

                    if upvalue.closed.is_none() {
                        self.stack[upvalue.location] = value;
                    } else {
                        upvalue.closed = Some(value);
                    }
                }
                Op::GetProperty(index) => {
                    if let Value::Instance(instance_id) = self.peek(0) {
                        let name_id = self.current_chunk().read_string(index);
                        let instance = self.memory.deref(instance_id).as_instance().unwrap();
                        let value = instance.fields.get(&name_id);

                        match value {
                            Some(&value) => {
                                self.pop();
                                self.push(value);
                            }
                            None => {
                                let class = instance.class;
                                self.bind_method(class, name_id)?;
                            }
                        }
                    } else {
                        return Err(self.runtime_error("Only instances have properties."));
                    }
                }
                Op::SetProperty(index) => {
                    if let Value::Instance(instance_id) = self.peek(1) {
                        let name = self.current_chunk().read_string(index);
                        let value = self.pop();
                        let instance = self
                            .memory
                            .deref_mut(instance_id)
                            .as_instance_mut()
                            .unwrap();

                        instance.fields.insert(name, value);

                        self.pop();
                        self.push(value);
                    } else {
                        return Err(self.runtime_error("Only instances have fields."));
                    }
                }
                Op::Equal => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(Value::Bool(a == b));
                }
                Op::GetSuper(index) => {
                    let name = self.current_chunk().read_string(index);

                    if let Value::Class(superclass) = self.pop() {
                        self.bind_method(superclass, name)?;
                    } else {
                        panic!("Tried to call super on a non-class.");
                    }
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
                    let value = self.pop();
                    self.display_value(value);
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
                Op::Invoke(index, arg_count) => {
                    let name = self.current_chunk().read_string(index);
                    self.invoke(name, arg_count as usize)?;
                }
                Op::SuperInvoke(index, arg_count) => {
                    let name = self.current_chunk().read_string(index);

                    if let Value::Class(superclass) = self.pop() {
                        self.invoke_from_class(superclass, name, arg_count as usize)?;
                    } else {
                        panic!("Tried to super invoke on a non-class.");
                    }
                }
                Op::Closure(index) => {
                    let constant = self.current_chunk().read_constant(index);

                    if let Value::Function(fun_id) = constant {
                        // TODO: double lookup?
                        let upvalues = self
                            .memory
                            .deref(fun_id)
                            .as_function()
                            .unwrap()
                            .upvalues
                            .len();
                        let mut closure = Closure::new(fun_id);

                        for upvalue in 0..upvalues {
                            let upvalue =
                                &self.memory.deref(fun_id).as_function().unwrap().upvalues[upvalue];
                            let obj_upvalue = if upvalue.is_local {
                                let location = self.current_frame().slot + upvalue.index as usize;
                                self.capture_upvalue(location)
                            } else {
                                self.current_closure().upvalues[upvalue.index as usize]
                            };

                            closure.upvalues.push(obj_upvalue);
                        }

                        let closure_id = self.alloc(ObjData::Closure(closure));
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
                    let frame = self.frames.pop().expect("non-empty call stack");
                    self.close_upvalues(frame.slot);

                    let result = self.pop();

                    if self.frames.is_empty() {
                        return Ok(());
                    }

                    self.stack.truncate(frame.slot);
                    self.push(result);
                }
                Op::Class(index) => {
                    let class_name = self.current_chunk().read_string(index);
                    let class = Class::new(class_name);
                    let class_id = self.alloc(ObjData::Class(class));
                    self.stack.push(Value::Class(class_id));
                }
                Op::Inherit => {
                    let subclass = self.peek(0);
                    let superclass = self.peek(1);

                    if let (Value::Class(subclass_id), Value::Class(superclass_id)) =
                        (subclass, superclass)
                    {
                        let superclass =
                            self.memory.deref_mut(superclass_id).as_class_mut().unwrap();
                        let methods = superclass.methods.clone();
                        let subclass = self.memory.deref_mut(subclass_id).as_class_mut().unwrap();

                        subclass.methods.extend(methods.iter());

                        self.pop();
                    } else {
                        return Err(self.runtime_error("Superclass must be a class."));
                    }
                }
                Op::Method(index) => {
                    let method_name = self.current_chunk().read_string(index);
                    self.define_method(method_name);
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
        self.stack.pop().expect("non-empty value stack")
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
            let closure = self.memory.deref(frame.closure_id).as_closure().unwrap();
            let function = self.memory.deref(closure.fun_id).as_function().unwrap();
            let line = frame.ip - 1;

            match function.name {
                Some(str_id) => {
                    let function_name = self.memory.deref(str_id).as_string().unwrap();
                    eprintln!(
                        "[line {}] in {}()",
                        function.chunk.line_at(line),
                        function_name
                    );
                }
                None => eprintln!("[line {}] in script", function.chunk.line_at(line)),
            };
        });

        LoxError::RuntimeError
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().expect("non-empty call stack")
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("non-empty call stack")
    }

    fn current_closure(&self) -> &Closure {
        let closure_id = self.current_frame().closure_id;
        self.memory.deref(closure_id).as_closure().unwrap()
    }

    fn current_chunk(&self) -> &Chunk {
        let closure = self.current_closure();
        let function = self.memory.deref(closure.fun_id).as_function().unwrap();
        &function.chunk
    }

    fn call_value(&mut self, arg_count: usize) -> Result<(), LoxError> {
        match self.peek(arg_count) {
            Value::BoundMethod(bound_id) => {
                let bound = self.memory.deref(bound_id).as_bound_method().unwrap();
                let bound_method = bound.method;
                let location = self.stack.len() - arg_count - 1;

                self.stack[location] = bound.receiver;
                self.call(bound_method, arg_count)
            }
            Value::Class(class_id) => {
                let instance = Instance::new(class_id);
                let instance_id = self.alloc(ObjData::Instance(instance));

                let location = self.stack.len() - arg_count - 1;
                self.stack[location] = Value::Instance(instance_id);

                let class = self.memory.deref(class_id).as_class().unwrap();
                if let Some(&initializer) = class.methods.get(&self.init_string) {
                    if let Value::Closure(closure_id) = initializer {
                        self.call(closure_id, arg_count)
                    } else {
                        panic!("Initializer should be a closure")
                    }
                } else if arg_count != 0 {
                    let message = format!("Expected 0 arguments but got {}.", arg_count);
                    Err(self.runtime_error(&message))
                } else {
                    Ok(())
                }
            }
            Value::Closure(closure_id) => self.call(closure_id, arg_count),
            Value::NativeFunction(native) => {
                let left = self.stack.len() - arg_count;
                let result = native.0(self, &self.stack[left..]);
                self.stack.truncate(left - 1);
                self.push(result);
                Ok(())
            }
            _ => Err(self.runtime_error("Can only call functions and classes.")),
        }
    }

    fn call(&mut self, closure_id: HeapId, arg_count: usize) -> Result<(), LoxError> {
        let closure = self.memory.deref(closure_id).as_closure().unwrap();
        let function = self.memory.deref(closure.fun_id).as_function().unwrap();

        if arg_count != function.arity {
            let message = format!(
                "Expected {} arguments but got {}.",
                function.arity, arg_count
            );

            Err(self.runtime_error(&message))
        } else if self.frames.len() == Self::FRAMES_MAX {
            Err(self.runtime_error("Stack overflow."))
        } else {
            let frame = CallFrame::new(closure_id, self.stack.len() - arg_count - 1);
            self.frames.push(frame);

            Ok(())
        }
    }

    fn define_native(&mut self, name: &str, native: NativeFunction) {
        let name = self.intern(name);
        self.globals.insert(name, Value::NativeFunction(native));
    }

    fn define_method(&mut self, name: HeapId) {
        if let Value::Class(class) = self.peek(1) {
            let method = self.peek(0);
            let class = self.memory.deref_mut(class).as_class_mut().unwrap();

            class.methods.insert(name, method);
            self.pop();
        } else {
            panic!("Cannot define a method on a non-class.");
        }
    }

    fn capture_upvalue(&mut self, location: usize) -> HeapId {
        let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalue;

        // TODO: double lookup
        while let Some(inner) = upvalue
            .filter(|upvalue| self.memory.deref(*upvalue).as_upvalue().unwrap().location > location)
        {
            upvalue = self.memory.deref(inner).as_upvalue().unwrap().next;
            prev_upvalue = Some(inner);
        }

        if let Some(inner) = upvalue.filter(|upvalue| {
            self.memory.deref(*upvalue).as_upvalue().unwrap().location == location
        }) {
            return inner;
        }

        let mut created_upvalue = Upvalue::new(location);
        created_upvalue.next = upvalue;
        let upvalue_id = self.alloc(ObjData::Upvalue(created_upvalue));

        if let Some(inner) = prev_upvalue {
            self.memory.deref_mut(inner).as_upvalue_mut().unwrap().next = Some(upvalue_id);
        } else {
            self.open_upvalue = Some(upvalue_id);
        }

        upvalue_id
    }

    fn close_upvalues(&mut self, last: usize) {
        while let Some(upvalue) = self
            .open_upvalue
            .filter(|upvalue| self.memory.deref(*upvalue).as_upvalue().unwrap().location >= last)
        {
            let mut upvalue = self.memory.deref_mut(upvalue).as_upvalue_mut().unwrap();
            let value = self.stack[upvalue.location];
            upvalue.closed = Some(value);
            self.open_upvalue = upvalue.next.take();
        }
    }

    fn alloc(&mut self, data: ObjData) -> HeapId {
        if self.memory.should_gc() {
            self.mark_and_sweep();
        }

        self.memory.alloc(data)
    }

    fn intern(&mut self, name: &str) -> HeapId {
        if self.memory.should_gc() {
            self.mark_and_sweep();
        }

        self.memory.intern(name)
    }

    fn mark_and_sweep(&mut self) {
        self.mark_roots();
        self.memory.collect_garbage();
    }

    fn mark_roots(&mut self) {
        self.stack
            .iter()
            .for_each(|value| self.memory.mark_value(*value));

        self.memory.mark_table(&self.globals);

        self.frames.iter().for_each(|frame| {
            self.memory.mark_object(frame.closure_id);
        });

        let mut upvalue = self.open_upvalue;
        while let Some(inner) = upvalue {
            self.memory.mark_object(inner);
            upvalue = self.memory.deref(inner).as_upvalue().unwrap().next;
        }

        self.memory.mark_object(self.init_string);
    }

    fn bind_method(&mut self, class: HeapId, name_id: HeapId) -> Result<(), LoxError> {
        let class = self.memory.deref(class).as_class().unwrap();

        match class.methods.get(&name_id) {
            Some(method_val) => {
                let receiver = self.peek(0);

                if let Value::Closure(method) = method_val {
                    let bound = BoundMethod::new(receiver, *method);
                    let bound_id = self.alloc(ObjData::BoundMethod(bound));

                    self.pop();
                    self.push(Value::BoundMethod(bound_id));

                    Ok(())
                } else {
                    panic!("Expected method to be a closure.");
                }
            }
            None => {
                let name = self.memory.deref(name_id).as_string().unwrap();
                let message = format!("Undefined property '{}'.", name);
                Err(self.runtime_error(&message))
            }
        }
    }

    fn display_value(&self, value: Value) {
        match value {
            Value::Bool(bool) => println!("{}", bool),
            Value::Nil => println!("nil"),
            Value::Number(num) => println!("{}", num),
            Value::String(str_id) => {
                println!("{}", self.memory.deref(str_id).as_string().unwrap());
            }
            Value::Function(fun_id) => {
                let fn_name = self.memory.deref(fun_id).as_function().unwrap().name;
                match fn_name {
                    Some(str_id) => {
                        println!("<fn {}>", self.memory.deref(str_id).as_string().unwrap());
                    }
                    None => panic!("Expected function name"),
                }
            }
            Value::NativeFunction(_) => println!("<native fn>"),
            Value::Closure(closure_id) => {
                let fun_id = self.memory.deref(closure_id).as_closure().unwrap().fun_id;
                self.display_value(Value::Function(fun_id));
            }
            Value::Class(class_id) => {
                let name_id = self.memory.deref(class_id).as_class().unwrap().name;
                self.display_value(Value::String(name_id));
            }
            Value::Instance(instance_id) => {
                let class_id = self.memory.deref(instance_id).as_instance().unwrap().class;
                let name_id = self.memory.deref(class_id).as_class().unwrap().name;
                let class_name = self.memory.deref(name_id).as_string().unwrap();
                println!("{} instance", class_name);
            }
            Value::BoundMethod(bound_id) => {
                let closure_id = self
                    .memory
                    .deref(bound_id)
                    .as_bound_method()
                    .unwrap()
                    .method;
                self.display_value(Value::Closure(closure_id));
            }
        }
    }

    fn invoke(&mut self, name: HeapId, arg_count: usize) -> Result<(), LoxError> {
        let receiver = self.peek(arg_count);
        match receiver {
            Value::Instance(instance_id) => {
                let instance = self.memory.deref(instance_id).as_instance().unwrap();

                match instance.fields.get(&name) {
                    Some(field) => {
                        let location = self.stack.len() - arg_count - 1;
                        self.stack[location] = *field;
                        self.call_value(arg_count)
                    }
                    None => {
                        let class = instance.class;
                        self.invoke_from_class(class, name, arg_count)
                    }
                }
            }
            _ => Err(self.runtime_error("Only instances have methods.")),
        }
    }

    fn invoke_from_class(
        &mut self,
        class: HeapId,
        name_id: HeapId,
        arg_count: usize,
    ) -> Result<(), LoxError> {
        let class = self.memory.deref(class).as_class().unwrap();

        if let Some(&method) = class.methods.get(&name_id) {
            if let Value::Closure(closure) = method {
                self.call(closure, arg_count)
            } else {
                panic!("Got method that is not closure!")
            }
        } else {
            let name = self.memory.deref(name_id).as_string().unwrap();
            let message = format!("Undefined property '{}'.", name);
            Err(self.runtime_error(&message))
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

fn clock_native(vm: &Vm, _args: &[Value]) -> Value {
    let time = vm
        .start_time
        .expect("start time is initiazed")
        .elapsed()
        .as_secs_f64();

    Value::Number(time)
}
