pub enum LoxError {
    Compile,
    Runtime,
}

pub struct CompileError(pub String);
pub struct RuntimeError;

pub type CompileResult<T> = Result<T, CompileError>;
pub type RuntimeResult = Result<(), RuntimeError>;
