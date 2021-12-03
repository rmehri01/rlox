#[derive(Debug, Clone, Copy)]
pub(crate) enum Value {
    Bool(bool),
    Nil,
    Number(f64),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Operation {
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
    code: Vec<Operation>,
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

    pub(crate) fn read(&self, index: usize) -> Operation {
        self.code[index]
    }

    pub(crate) fn write(&mut self, operation: Operation, line: usize) {
        self.code.push(operation);
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
