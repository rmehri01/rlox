use crate::{
    chunk::{Chunk, Operation},
    vm::Vm,
};

mod chunk;
mod error;
mod vm;

fn main() {
    let mut chunk = Chunk::new();

    let constant = chunk.add_constant(1.2);
    chunk.write(Operation::Constant(constant.try_into().unwrap()), 123);

    let constant = chunk.add_constant(3.4);
    chunk.write(Operation::Constant(constant.try_into().unwrap()), 123);

    chunk.write(Operation::Add, 123);

    let constant = chunk.add_constant(5.6);
    chunk.write(Operation::Constant(constant.try_into().unwrap()), 123);

    chunk.write(Operation::Divide, 123);
    chunk.write(Operation::Negate, 123);
    chunk.write(Operation::Return, 123);
    println!("{:?}", chunk);

    Vm::new(chunk).run();
}
