use core::fmt;
use std::ptr;

use arrayvec::ArrayVec;

use crate::{
    chunk::{Chunk, Value},
    compiler::Compiler,
    interner::StrId,
};

#[derive(Debug, Clone)]
pub(crate) struct Closure {
    pub(crate) fun_id: FunId,
    pub(crate) upvalues: Vec<Upvalue>, // TODO: gc id
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.fun_id == other.fun_id
    }
}

impl Closure {
    pub(crate) fn new(fun_id: FunId) -> Self {
        Self {
            fun_id,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Upvalue {
    pub(crate) location: usize,
}

impl Upvalue {
    pub(crate) fn new(location: usize) -> Self {
        Self { location }
    }
}

#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) arity: usize,
    pub(crate) chunk: Chunk,
    pub(crate) name: Option<StrId>,
    pub(crate) upvalues: ArrayVec<FnUpvalue, { Compiler::MAX_LOCALS }>,
}

#[derive(Debug)]
pub(crate) struct FnUpvalue {
    pub(crate) index: u8,
    pub(crate) is_local: bool,
}

impl FnUpvalue {
    pub(crate) fn new(index: u8, is_local: bool) -> Self {
        Self { index, is_local }
    }
}

impl Function {
    pub(crate) fn new(name: Option<StrId>) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name,
            upvalues: ArrayVec::new(),
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) struct NativeFunction(pub(crate) fn(&[Value]) -> Value);

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
pub(crate) struct FunId(usize);

#[derive(Debug)]
pub(crate) struct Functions {
    functions: Vec<Function>,
}

impl Functions {
    pub(crate) fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub(crate) fn lookup(&self, fun_id: FunId) -> &Function {
        &self.functions[fun_id.0]
    }

    pub(crate) fn add(&mut self, function: Function) -> FunId {
        self.functions.push(function);
        FunId(self.functions.len() - 1)
    }
}
