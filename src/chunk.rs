use crate::{
    interner::StrId,
    object::{ClosureId, FunId, NativeFunction},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(StrId),
    Function(FunId),
    NativeFunction(NativeFunction),
    Closure(ClosureId),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Bool(false) | Value::Nil)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
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
    GetUpvalue(u8),
    SetUpvalue(u8),
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
    Closure(u8),
    CloseUpvalue,
    Return,
}

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<Op>,
    constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn read(&self, index: usize) -> Op {
        self.code[index]
    }

    pub fn write(&mut self, op: Op, line: usize) {
        self.code.push(op);
        self.lines.push(line);
    }

    pub fn read_constant(&self, index: u8) -> Value {
        self.constants[index as usize]
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
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
