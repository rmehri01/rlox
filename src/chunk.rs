use crate::interner::StrId;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(StrId),
}

impl Value {
    pub(crate) fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(value) => !value,
            Value::Nil => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Op {
    Constant(u8),
    Nil,
    True,
    False,
    Pop,
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
    Return,
}

#[derive(Debug)]
pub(crate) struct Chunk {
    code: Vec<Op>,
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
}
