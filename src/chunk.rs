use crate::{
    interner::StrId,
    object::{FunId, NativeFunction},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(StrId),
    Function(FunId),
    NativeFunction(NativeFunction),
}

impl Value {
    pub(crate) fn is_falsey(&self) -> bool {
        matches!(self, Value::Bool(false) | Value::Nil)
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Op {
    Constant(u8),
    Nil,
    True,
    False,
    Pop,
    GetLocal(u8),
    SetLocal(u8),
    GetGlobal(u8),
    DefineGlobal(u8),
    SetGlobal(u8),
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump(u16),
    JumpIfFalse(u16),
    Loop(u16),
    Call(u8),
    Return,
}

#[derive(Debug)]
pub(crate) struct Chunk {
    pub(crate) code: Vec<Op>,
    constants: Vec<Value>,
    pub(crate) lines: Vec<usize>,
}

impl Chunk {
    pub(crate) fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub(crate) fn read(&self, index: usize) -> Op {
        self.code[index]
    }

    pub(crate) fn write(&mut self, op: Op, line: usize) {
        self.code.push(op);
        self.lines.push(line);
    }

    pub(crate) fn read_constant(&self, index: u8) -> Value {
        self.constants[index as usize]
    }

    pub(crate) fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn read_string(&self, index: u8) -> StrId {
        if let Value::String(s) = self.read_constant(index) {
            s
        } else {
            panic!("Constant is not string.")
        }
    }

    pub fn last_index(&self) -> usize {
        self.code.len() - 1
    }
}
