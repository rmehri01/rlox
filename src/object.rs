use core::fmt;
use std::ptr;

use crate::{
    chunk::{Chunk, Value},
    memory::HeapId,
    vm::Vm,
};

#[derive(Debug)]
pub struct Object {
    pub data: ObjData,
    is_marked: bool,
}

impl Object {
    pub fn new(data: ObjData) -> Self {
        Self {
            data,
            is_marked: false,
        }
    }
}

#[derive(Debug)]
pub enum ObjData {
    String(String),
    Function(Function),
    Closure(Closure),
    Upvalue(Upvalue),
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub fun_id: HeapId,
    pub upvalues: Vec<HeapId>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.fun_id == other.fun_id
    }
}

impl Closure {
    pub fn new(fun_id: HeapId) -> Self {
        Self {
            fun_id,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    pub location: usize,
    pub next: Option<HeapId>,
    pub closed: Option<Value>,
}

impl Upvalue {
    pub fn new(location: usize) -> Self {
        Self {
            location,
            next: None,
            closed: None,
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<HeapId>,
    pub upvalues: Vec<FnUpvalue>,
}

#[derive(Debug)]
pub struct FnUpvalue {
    pub index: u8,
    pub is_local: bool,
}

impl FnUpvalue {
    pub fn new(index: u8, is_local: bool) -> Self {
        Self { index, is_local }
    }
}

impl Function {
    pub fn new(name: Option<HeapId>) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct NativeFunction(pub fn(vm: &Vm, &[Value]) -> Value);

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other)
    }
}

#[macro_export]
macro_rules! cast {
    ($target: expr, $pat: path) => {{
        if let $pat(a) = $target {
            a
        } else {
            panic!("mismatch variant when cast to {}", stringify!($pat));
        }
    }};
}
