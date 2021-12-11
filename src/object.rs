use core::fmt;
use std::ptr;

use crate::{
    chunk::{Chunk, Value},
    interner::StrId,
};

#[derive(Debug)]
pub(crate) struct Function {
    pub(crate) arity: usize,
    pub(crate) chunk: Chunk,
    pub(crate) name: Option<StrId>,
}

impl Function {
    pub(crate) fn new(name: Option<StrId>) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name,
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
