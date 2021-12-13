use core::fmt;
use std::ptr;

use arrayvec::ArrayVec;

use crate::{
    chunk::{Chunk, Value},
    compiler::Compiler,
    interner::StrId,
};

#[derive(Debug, Clone)]
pub struct Closure {
    pub fun_id: FunId,
    pub upvalues: Vec<Upvalue>, // TODO: gc id
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.fun_id == other.fun_id
    }
}

impl Closure {
    pub fn new(fun_id: FunId) -> Self {
        Self {
            fun_id,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    pub location: usize,
}

impl Upvalue {
    pub fn new(location: usize) -> Self {
        Self { location }
    }
}

#[derive(Debug)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<StrId>,
    pub upvalues: ArrayVec<FnUpvalue, { Compiler::MAX_LOCALS }>,
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
    pub fn new(name: Option<StrId>) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name,
            upvalues: ArrayVec::new(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct NativeFunction(pub fn(&[Value]) -> Value);

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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FunId(usize);

#[derive(Debug)]
pub struct Functions {
    functions: Vec<Function>,
}

impl Functions {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn lookup(&self, fun_id: FunId) -> &Function {
        &self.functions[fun_id.0]
    }

    pub fn add(&mut self, function: Function) -> FunId {
        self.functions.push(function);
        FunId(self.functions.len() - 1)
    }
}
