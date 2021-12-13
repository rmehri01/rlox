pub(crate) enum LoxError {
    Compile,
    Runtime,
}

pub(crate) struct CompileError(pub(crate) String);
pub(crate) struct RuntimeError;

pub(crate) type CompileResult<T> = Result<T, CompileError>;
pub(crate) type RuntimeResult = Result<(), RuntimeError>;
