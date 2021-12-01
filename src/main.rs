use crate::{
    chunk::{Chunk, OpCode},
    vm::Vm,
};

mod chunk;
mod error;
mod vm;

fn main() {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(1.2);
    chunk.write(OpCode::Constant(constant.try_into().unwrap()), 123);

    let constant = chunk.add_constant(3.4);
    chunk.write(OpCode::Constant(constant.try_into().unwrap()), 123);

    chunk.write(OpCode::Add, 123);

    let constant = chunk.add_constant(5.6);
    chunk.write(OpCode::Constant(constant.try_into().unwrap()), 123);

    chunk.write(OpCode::Divide, 123);
    chunk.write(OpCode::Negate, 123);
    chunk.write(OpCode::Return, 123);
    println!("{:?}", chunk);

    Vm::new(chunk).run();
}
