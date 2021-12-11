use crate::{chunk::Chunk, interner::StrId};

#[derive(Debug)]
pub(crate) struct Function {
    arity: usize,
    pub(crate) chunk: Chunk,
    pub(crate) name: StrId,
}

impl Function {
    pub(crate) fn new() -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name: StrId(0), // TODO: not null?
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct FunId(usize);

pub(crate) struct Functions {
    functions: Vec<Function>,
}

impl Functions {
    pub(crate) fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub(crate) fn lookup(&self, fn_id: FunId) -> &Function {
        &self.functions[fn_id.0]
    }

    pub(crate) fn add(&mut self, function: Function) -> FunId {
        self.functions.push(function);
        FunId(self.functions.len() - 1)
    }
}
