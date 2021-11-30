use crate::chunk::{Chunk, OpCode};

mod chunk;

fn main() {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(1.2);
    chunk.write(OpCode::Constant(constant.try_into().unwrap()), 123);

    chunk.write(OpCode::Return, 123);
    println!("{:?}", chunk);
}
