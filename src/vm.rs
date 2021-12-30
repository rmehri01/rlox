use std::{mem, time::Instant};

use arrayvec::ArrayVec;
use rustc_hash::FxHashMap;

use crate::{
    cast,
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
                Op::Constant(index) => self.op_constant(index)?,
                Op::Add => self.op_add()?,
                Op::Nil => self.push(Value::Nil),
                Op::True => self.push(Value::Bool(true)),
                Op::False => self.push(Value::Bool(false)),
                Op::Pop => self.op_pop()?,
                Op::GetLocal(index) => self.op_get_local(index)?,
                Op::SetLocal(index) => self.op_set_local(index)?,
                Op::GetGlobal(index) => self.op_get_global(index)?,
                Op::DefineGlobal(index) => self.op_define_global(index)?,
                Op::SetGlobal(index) => self.op_set_global(index)?,
                Op::GetUpvalue(slot) => self.op_get_upvalue(slot)?,
                Op::SetUpvalue(slot) => self.op_set_upvalue(slot)?,
                Op::GetProperty(index) => self.op_get_property(index)?,
                Op::SetProperty(index) => self.op_set_property(index)?,
                Op::Equal => self.op_equal()?,
                Op::GetSuper(index) => self.op_get_super(index)?,
                Op::Greater => self.binary_op(|a, b| a > b, Value::Bool)?,
                Op::Less => self.binary_op(|a, b| a < b, Value::Bool)?,
                Op::Subtract => self.binary_op(|a, b| a - b, Value::Number)?,
                Op::Multiply => self.binary_op(|a, b| a * b, Value::Number)?,
                Op::Divide => self.binary_op(|a, b| a / b, Value::Number)?,
                Op::Not => self.op_not()?,
                Op::Negate => self.op_negate()?,
                Op::Print => self.op_print()?,
                Op::Jump(offset) => self.op_jump(offset)?,
                Op::JumpIfFalse(offset) => self.op_jump_if_false(offset)?,
                Op::Loop(offset) => self.op_loop(offset)?,
                Op::Call(arg_count) => self.call_value(arg_count as usize)?,
                Op::Invoke(index, arg_count) => self.op_invoke(index, arg_count)?,
                Op::SuperInvoke(index, arg_count) => self.op_super_invoke(index, arg_count)?,
                Op::Closure(index) => self.op_closure(index)?,
                Op::CloseUpvalue => self.op_close_upvalue()?,
                Op::Return => {
                    self.op_return()?;

                    if self.frames.is_empty() {
                        return Ok(());
                    }
                }
                Op::Class(index) => self.op_class(index)?,
                Op::Inherit => self.op_inherit()?,
                Op::Method(index) => self.op_method(index)?,
            }
        }
    }

    fn op_constant(&mut self, index: u8) -> Result<(), LoxError> {
        let constant = self.current_chunk().read_constant(index);
        self.push(constant);

        Ok(())
    }

    fn op_add(&mut self) -> Result<(), LoxError> {
        let (b, a) = (self.pop(), self.pop());

        match (a, b) {
            (Value::String(a), Value::String(b)) => {
                let str_a = cast!(self.memory.deref(a), ObjData::String);
                let str_b = cast!(self.memory.deref(b), ObjData::String);

                let result = str_a.clone() + str_b;
                let result_id = self.intern(&result);

                self.push(Value::String(result_id));
                Ok(())
            }
            (Value::Number(a), Value::Number(b)) => {
                self.push(Value::Number(a + b));
                Ok(())
            }
            _ => Err(self.runtime_error("Operands must be two numbers or two strings.")),
        }
    }

    fn op_pop(&mut self) -> Result<(), LoxError> {
        self.pop();
        Ok(())
    }

    fn op_get_local(&mut self, index: u8) -> Result<(), LoxError> {
        let slot = self.current_frame().slot + index as usize;
        let value = self.stack[slot];
        self.push(value);

        Ok(())
    }

    fn op_set_local(&mut self, index: u8) -> Result<(), LoxError> {
        let slot = self.current_frame().slot + index as usize;
        self.stack[slot] = self.peek(0);

        Ok(())
    }

    fn op_get_global(&mut self, index: u8) -> Result<(), LoxError> {
        let str_id = self.current_chunk().read_string(index);

        self.globals
            .get(&str_id)
            .map(|value| self.stack.push(*value))
            .ok_or_else(|| {
                let name = cast!(self.memory.deref(str_id), ObjData::String);
                let message = format!("Undefined variable '{}'.", name);
                self.runtime_error(&message)
            })
    }

    fn op_define_global(&mut self, index: u8) -> Result<(), LoxError> {
        let str_id = self.current_chunk().read_string(index);
        let name = self.pop();
        self.globals.insert(str_id, name);

        Ok(())
    }

    fn op_set_global(&mut self, index: u8) -> Result<(), LoxError> {
        let str_id = self.current_chunk().read_string(index);
        let value = self.peek(0);

        self.globals
            .insert(str_id, value)
            .map(|_| ())
            .ok_or_else(|| {
                self.globals.remove(&str_id);
                let name = cast!(self.memory.deref(str_id), ObjData::String);
                let message = format!("Undefined variable '{}'.", name);
                self.runtime_error(&message)
            })
    }

    fn op_get_upvalue(&mut self, slot: u8) -> Result<(), LoxError> {
        let value = {
            let current_closure = self.current_closure();
            let upvalue_id = current_closure.upvalues[slot as usize];
            let upvalue = cast!(self.memory.deref(upvalue_id), ObjData::Upvalue);

            match upvalue.closed {
                Some(value) => value,
                None => self.stack[upvalue.location],
            }
        };

        self.push(value);
        Ok(())
    }

    fn op_set_upvalue(&mut self, slot: u8) -> Result<(), LoxError> {
        let value = self.peek(0);
        let upvalue_id = self.current_closure().upvalues[slot as usize];
        let mut upvalue = cast!(self.memory.deref_mut(upvalue_id), ObjData::Upvalue);

        if upvalue.closed.is_none() {
            self.stack[upvalue.location] = value;
        } else {
            upvalue.closed = Some(value);
        }

        Ok(())
    }

    fn op_get_property(&mut self, index: u8) -> Result<(), LoxError> {
        if let Value::Instance(instance_id) = self.peek(0) {
            let name_id = self.current_chunk().read_string(index);
            let instance = cast!(self.memory.deref(instance_id), ObjData::Instance);
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

            Ok(())
        } else {
            Err(self.runtime_error("Only instances have properties."))
        }
    }

    fn op_set_property(&mut self, index: u8) -> Result<(), LoxError> {
        if let Value::Instance(instance_id) = self.peek(1) {
            let name = self.current_chunk().read_string(index);
            let value = self.pop();
            let instance = cast!(self.memory.deref_mut(instance_id), ObjData::Instance);

            instance.fields.insert(name, value);

            self.pop();
            self.push(value);

            Ok(())
        } else {
            Err(self.runtime_error("Only instances have fields."))
        }
    }

    fn op_equal(&mut self) -> Result<(), LoxError> {
        let b = self.pop();
        let a = self.pop();

        self.push(Value::Bool(a == b));
        Ok(())
    }

    fn op_get_super(&mut self, index: u8) -> Result<(), LoxError> {
        let name = self.current_chunk().read_string(index);
        let superclass = cast!(self.pop(), Value::Class);

        self.bind_method(superclass, name)
    }

    fn op_not(&mut self) -> Result<(), LoxError> {
        let value = self.pop();
        self.push(Value::Bool(value.is_falsey()));
        Ok(())
    }

    fn op_negate(&mut self) -> Result<(), LoxError> {
        match self.peek(0) {
            Value::Number(value) => {
                self.pop();
                self.push(Value::Number(-value));
                Ok(())
            }
            _ => Err(self.runtime_error("Operand must be a number.")),
        }
    }

    fn op_print(&mut self) -> Result<(), LoxError> {
        let value = self.pop();
        self.display_value(value);
        Ok(())
    }

    fn op_jump(&mut self, offset: u16) -> Result<(), LoxError> {
        self.current_frame_mut().ip += offset as usize;
        Ok(())
    }

    fn op_jump_if_false(&mut self, offset: u16) -> Result<(), LoxError> {
        if self.peek(0).is_falsey() {
            self.op_jump(offset)
        } else {
            Ok(())
        }
    }

    fn op_loop(&mut self, offset: u16) -> Result<(), LoxError> {
        self.current_frame_mut().ip -= offset as usize;
        Ok(())
    }

    fn op_invoke(&mut self, index: u8, arg_count: u8) -> Result<(), LoxError> {
        let name = self.current_chunk().read_string(index);
        self.invoke(name, arg_count as usize)
    }

    fn op_super_invoke(&mut self, index: u8, arg_count: u8) -> Result<(), LoxError> {
        let name = self.current_chunk().read_string(index);
        let superclass = cast!(self.pop(), Value::Class);

        self.invoke_from_class(superclass, name, arg_count as usize)
    }

    fn op_closure(&mut self, index: u8) -> Result<(), LoxError> {
        let constant = self.current_chunk().read_constant(index);
        let fun_id = cast!(constant, Value::Function);

        let function = cast!(self.memory.deref_mut(fun_id), ObjData::Function);
        let upvalues = mem::take(&mut function.upvalues);
        let mut closure = Closure::new(fun_id);

        upvalues.iter().for_each(|upvalue| {
            let obj_upvalue = if upvalue.is_local {
                let location = self.current_frame().slot + upvalue.index as usize;
                self.capture_upvalue(location)
            } else {
                self.current_closure().upvalues[upvalue.index as usize]
            };

            closure.upvalues.push(obj_upvalue);
        });

        cast!(self.memory.deref_mut(fun_id), ObjData::Function).upvalues = upvalues;

        let closure_id = self.alloc(ObjData::Closure(closure));
        self.push(Value::Closure(closure_id));
        Ok(())
    }

    fn op_close_upvalue(&mut self) -> Result<(), LoxError> {
        self.close_upvalues(self.stack.len() - 1);
        self.pop();
        Ok(())
    }

    fn op_return(&mut self) -> Result<(), LoxError> {
        let frame = self.frames.pop().expect("non-empty call stack");
        self.close_upvalues(frame.slot);

        let result = self.pop();

        if !self.frames.is_empty() {
            self.stack.truncate(frame.slot);
            self.push(result);
        }

        Ok(())
    }

    fn op_class(&mut self, index: u8) -> Result<(), LoxError> {
        let class_name = self.current_chunk().read_string(index);
        let class = Class::new(class_name);
        let class_id = self.alloc(ObjData::Class(class));

        self.stack.push(Value::Class(class_id));
        Ok(())
    }

    fn op_inherit(&mut self) -> Result<(), LoxError> {
        let subclass = self.peek(0);
        let superclass = self.peek(1);

        if let (Value::Class(subclass_id), Value::Class(superclass_id)) = (subclass, superclass) {
            let superclass = cast!(self.memory.deref_mut(superclass_id), ObjData::Class);
            let methods = superclass.methods.clone();
            let subclass = cast!(self.memory.deref_mut(subclass_id), ObjData::Class);

            subclass.methods.extend(methods.iter());

            self.pop();
            Ok(())
        } else {
            Err(self.runtime_error("Superclass must be a class."))
        }
    }

    fn op_method(&mut self, index: u8) -> Result<(), LoxError> {
        let method_name = self.current_chunk().read_string(index);

        self.define_method(method_name);
        Ok(())
    }

    fn binary_op<T>(
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
        let frame = self.current_frame_mut();
        let fun_id = frame.fun_id;
        let ip = frame.ip;

        frame.ip += 1;
        cast!(self.memory.deref(fun_id), ObjData::Function)
            .chunk
            .read(ip)
    }

    fn peek(&self, distance: usize) -> Value {
        let size = self.stack.len();
        self.stack[size - 1 - distance]
    }

    fn runtime_error(&self, message: &str) -> LoxError {
        eprintln!("{}", message);

        self.frames.iter().rev().for_each(|frame| {
            let closure = cast!(self.memory.deref(frame.closure_id), ObjData::Closure);
            let function = cast!(self.memory.deref(closure.fun_id), ObjData::Function);
            let line = frame.ip - 1;

            match function.name {
                Some(str_id) => {
                    let function_name = cast!(self.memory.deref(str_id), ObjData::String);
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
        cast!(self.memory.deref(closure_id), ObjData::Closure)
    }

    fn current_chunk(&self) -> &Chunk {
        let fun_id = self.current_frame().fun_id;
        let function = cast!(self.memory.deref(fun_id), ObjData::Function);

        &function.chunk
    }

    fn call_value(&mut self, arg_count: usize) -> Result<(), LoxError> {
        match self.peek(arg_count) {
            Value::BoundMethod(bound_id) => {
                let bound = cast!(self.memory.deref(bound_id), ObjData::BoundMethod);
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

                let class = cast!(self.memory.deref(class_id), ObjData::Class);
                if let Some(&initializer) = class.methods.get(&self.init_string) {
                    let closure_id = cast!(initializer, Value::Closure);

                    self.call(closure_id, arg_count)
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
        let closure = cast!(self.memory.deref(closure_id), ObjData::Closure);
        let function = cast!(self.memory.deref(closure.fun_id), ObjData::Function);

        if arg_count != function.arity {
            let message = format!(
                "Expected {} arguments but got {}.",
                function.arity, arg_count
            );

            Err(self.runtime_error(&message))
        } else if self.frames.len() == Self::FRAMES_MAX {
            Err(self.runtime_error("Stack overflow."))
        } else {
            let frame =
                CallFrame::new(closure_id, closure.fun_id, self.stack.len() - arg_count - 1);
            self.frames.push(frame);

            Ok(())
        }
    }

    fn define_native(&mut self, name: &str, native: NativeFunction) {
        let name = self.intern(name);
        self.globals.insert(name, Value::NativeFunction(native));
    }

    fn define_method(&mut self, name: HeapId) {
        let class = cast!(self.peek(1), Value::Class);
        let method = self.peek(0);
        let class = cast!(self.memory.deref_mut(class), ObjData::Class);

        class.methods.insert(name, method);
        self.pop();
    }

    fn capture_upvalue(&mut self, location: usize) -> HeapId {
        let mut prev_upvalue = None;
        let mut upvalue = self.open_upvalue;

        while let Some(inner) = upvalue {
            let inner_upvalue = cast!(self.memory.deref(inner), ObjData::Upvalue);

            if inner_upvalue.location > location {
                upvalue = cast!(self.memory.deref(inner), ObjData::Upvalue).next;
                prev_upvalue = Some(inner);
            } else {
                break;
            }
        }

        if let Some(inner) = upvalue.filter(|upvalue| {
            cast!(self.memory.deref(*upvalue), ObjData::Upvalue).location == location
        }) {
            return inner;
        }

        let mut created_upvalue = Upvalue::new(location);
        created_upvalue.next = upvalue;
        let upvalue_id = self.alloc(ObjData::Upvalue(created_upvalue));

        if let Some(inner) = prev_upvalue {
            cast!(self.memory.deref_mut(inner), ObjData::Upvalue).next = Some(upvalue_id);
        } else {
            self.open_upvalue = Some(upvalue_id);
        }

        upvalue_id
    }

    fn close_upvalues(&mut self, last: usize) {
        while let Some(upvalue) = self.open_upvalue {
            let mut upvalue = cast!(self.memory.deref_mut(upvalue), ObjData::Upvalue);

            if upvalue.location >= last {
                let value = self.stack[upvalue.location];
                upvalue.closed = Some(value);
                self.open_upvalue = upvalue.next.take();
            } else {
                break;
            }
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
            upvalue = cast!(self.memory.deref(inner), ObjData::Upvalue).next;
        }

        self.memory.mark_object(self.init_string);
    }

    fn bind_method(&mut self, class: HeapId, name_id: HeapId) -> Result<(), LoxError> {
        let class = cast!(self.memory.deref(class), ObjData::Class);

        match class.methods.get(&name_id) {
            Some(method_val) => {
                let receiver = self.peek(0);
                let method = cast!(method_val, Value::Closure);

                let bound = BoundMethod::new(receiver, *method);
                let bound_id = self.alloc(ObjData::BoundMethod(bound));

                self.pop();
                self.push(Value::BoundMethod(bound_id));

                Ok(())
            }
            None => {
                let name = cast!(self.memory.deref(name_id), ObjData::String);
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
                println!("{}", cast!(self.memory.deref(str_id), ObjData::String));
            }
            Value::Function(fun_id) => {
                let fn_name = cast!(self.memory.deref(fun_id), ObjData::Function).name;
                match fn_name {
                    Some(str_id) => {
                        println!("<fn {}>", cast!(self.memory.deref(str_id), ObjData::String));
                    }
                    None => panic!("Expected function name"),
                }
            }
            Value::NativeFunction(_) => println!("<native fn>"),
            Value::Closure(closure_id) => {
                let fun_id = cast!(self.memory.deref(closure_id), ObjData::Closure).fun_id;
                self.display_value(Value::Function(fun_id));
            }
            Value::Class(class_id) => {
                let name_id = cast!(self.memory.deref(class_id), ObjData::Class).name;
                self.display_value(Value::String(name_id));
            }
            Value::Instance(instance_id) => {
                let class_id = cast!(self.memory.deref(instance_id), ObjData::Instance).class;
                let name_id = cast!(self.memory.deref(class_id), ObjData::Class).name;
                let class_name = cast!(self.memory.deref(name_id), ObjData::String);
                println!("{} instance", class_name);
            }
            Value::BoundMethod(bound_id) => {
                let closure_id = cast!(self.memory.deref(bound_id), ObjData::BoundMethod).method;
                self.display_value(Value::Closure(closure_id));
            }
        }
    }

    fn invoke(&mut self, name: HeapId, arg_count: usize) -> Result<(), LoxError> {
        let receiver = self.peek(arg_count);
        match receiver {
            Value::Instance(instance_id) => {
                let instance = cast!(self.memory.deref(instance_id), ObjData::Instance);

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
        let class = cast!(self.memory.deref(class), ObjData::Class);

        if let Some(&method) = class.methods.get(&name_id) {
            let closure = cast!(method, Value::Closure);

            self.call(closure, arg_count)
        } else {
            let name = cast!(self.memory.deref(name_id), ObjData::String);
            let message = format!("Undefined property '{}'.", name);
            Err(self.runtime_error(&message))
        }
    }
}

#[derive(Debug)]
struct CallFrame {
    closure_id: HeapId,
    fun_id: HeapId,
    ip: usize,
    slot: usize,
}

impl CallFrame {
    fn new(closure_id: HeapId, fun_id: HeapId, slot: usize) -> Self {
        Self {
            closure_id,
            fun_id,
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
