use core::fmt;
use std::ptr;

use enum_as_inner::EnumAsInner;
use rustc_hash::FxHashMap;

use crate::{
    chunk::{Chunk, Value},
    memory::HeapId,
    vm::Vm,
};

#[derive(Debug)]
pub struct Object {
    pub data: ObjData,
    pub is_marked: bool,
}

impl Object {
    pub fn new(data: ObjData) -> Self {
        Self {
            data,
            is_marked: false,
        }
    }
}

#[derive(Debug, EnumAsInner)]
pub enum ObjData {
    String(String),
    Function(Function),
    Closure(Closure),
    Upvalue(Upvalue),
    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
    None,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub fun_id: HeapId,
    pub upvalues: Vec<HeapId>,
}

impl Closure {
    pub fn new(fun_id: HeapId) -> Self {
        Self {
            fun_id,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Upvalue {
    pub location: usize,
    pub next: Option<HeapId>,
    pub closed: Option<Value>,
}

impl Upvalue {
    pub fn new(location: usize) -> Self {
        Self {
            location,
            next: None,
            closed: None,
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: Option<HeapId>,
    pub upvalues: Vec<FunctionUpvalue>,
}

#[derive(Debug)]
pub struct FunctionUpvalue {
    pub index: u8,
    pub is_local: bool,
}

impl FunctionUpvalue {
    pub fn new(index: u8, is_local: bool) -> Self {
        Self { index, is_local }
    }
}

impl Function {
    pub fn new(name: Option<HeapId>) -> Self {
        Self {
            arity: 0,
            chunk: Chunk::new(),
            name,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Clone, Copy)]
pub struct NativeFunction(pub fn(vm: &Vm, &[Value]) -> Value);

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self, other)
    }
}

#[derive(Debug)]
pub struct Class {
    pub name: HeapId,
    pub methods: FxHashMap<HeapId, Value>,
}

impl Class {
    pub fn new(name: HeapId) -> Self {
        Self {
            name,
            methods: FxHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct Instance {
    pub class: HeapId,
    pub fields: FxHashMap<HeapId, Value>,
}

impl Instance {
    pub fn new(class: HeapId) -> Self {
        Self {
            class,
            fields: FxHashMap::default(),
        }
    }
}

#[derive(Debug)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: HeapId,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: HeapId) -> Self {
        Self { receiver, method }
    }
}
