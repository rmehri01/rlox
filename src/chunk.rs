pub(crate) type Value = f64;

#[derive(Debug, Clone, Copy)]
pub(crate) enum OpCode {
    Constant(u8),
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
}

#[derive(Debug)]
pub(crate) struct Chunk {
    code: Vec<OpCode>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub(crate) fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub(crate) fn read(&self, index: usize) -> OpCode {
        self.code[index]
    }

    pub(crate) fn write(&mut self, op_code: OpCode, line: usize) {
        self.code.push(op_code);
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
