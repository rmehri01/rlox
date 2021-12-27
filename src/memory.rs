use std::{collections::VecDeque, mem};

use rustc_hash::FxHashMap;

use crate::{
    chunk::Value,
    object::{ObjData, Object},
};

// TODO: try arena
pub struct Memory {
    strings: FxHashMap<String, HeapId>,
    heap: Vec<Object>,
    free_list: Vec<HeapId>,
    gray_stack: VecDeque<HeapId>,
    bytes_allocated: usize,
    next_gc: usize,
}

impl Memory {
    const HEAP_GROW_FACTOR: usize = 2;

    pub fn new() -> Self {
        Self {
            strings: FxHashMap::default(),
            heap: Vec::new(),
            free_list: Vec::new(),
            gray_stack: VecDeque::new(),
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
        }
    }

    pub fn alloc(&mut self, data: ObjData) -> HeapId {
        self.bytes_allocated += mem::size_of_val(&data);
        let object = Object::new(data);

        if let Some(heap_id) = self.free_list.pop() {
            self.heap[heap_id.0] = object;
            heap_id
        } else {
            self.heap.push(object);
            HeapId(self.heap.len() - 1)
        }
    }

    pub fn deref(&self, heap_id: HeapId) -> &ObjData {
        &self.heap[heap_id.0].data
    }

    pub fn deref_mut(&mut self, heap_id: HeapId) -> &mut ObjData {
        &mut self.heap[heap_id.0].data
    }

    pub fn intern(&mut self, name: &str) -> HeapId {
        if let Some(&idx) = self.strings.get(name) {
            return idx;
        }

        let str_id = self.alloc(ObjData::String(name.to_string()));
        self.strings.insert(name.to_string(), str_id);

        str_id
    }

    pub fn should_gc(&self) -> bool {
        self.bytes_allocated > self.next_gc
    }

    fn free(&mut self, heap_id: HeapId) {
        self.bytes_allocated -= mem::size_of_val(&self.heap[heap_id.0].data);
        self.free_list.push(heap_id);
    }

    pub fn collect_garbage(&mut self) {
        self.trace_references();
        self.remove_white_strings();
        self.sweep();

        self.next_gc = self.bytes_allocated * Memory::HEAP_GROW_FACTOR;
    }

    fn trace_references(&mut self) {
        while let Some(heap_id) = self.gray_stack.pop_back() {
            self.blacken_object(heap_id);
        }
    }

    // TODO: better way of doing this?
    fn blacken_object(&mut self, heap_id: HeapId) {
        let obj_data = mem::replace(&mut self.heap[heap_id.0].data, ObjData::None);

        match &obj_data {
            ObjData::Function(function) => {
                if let Some(heap_id) = function.name {
                    self.mark_object(heap_id)
                }

                function
                    .chunk
                    .constants
                    .iter()
                    .for_each(|value| self.mark_value(*value));
            }
            ObjData::Closure(closure) => {
                self.mark_object(closure.fun_id);
                closure
                    .upvalues
                    .iter()
                    .for_each(|heap_id| self.mark_object(*heap_id));
            }
            ObjData::Upvalue(upvalue) => {
                if let Some(value) = upvalue.closed {
                    self.mark_value(value)
                }
            }
            ObjData::Class(class) => {
                self.mark_object(class.name);
                self.mark_table(&class.methods);
            }
            ObjData::Instance(instance) => {
                self.mark_object(instance.class);
                self.mark_table(&instance.fields);
            }
            ObjData::BoundMethod(bound_method) => {
                self.mark_value(bound_method.receiver);
                self.mark_object(bound_method.method);
            }
            _ => {}
        }

        self.heap[heap_id.0].data = obj_data;
    }

    pub fn mark_table(&mut self, table: &FxHashMap<HeapId, Value>) {
        table.iter().for_each(|(object, value)| {
            self.mark_object(*object);
            self.mark_value(*value);
        })
    }

    fn remove_white_strings(&mut self) {
        self.strings
            .retain(|_, heap_id| self.heap[heap_id.0].is_marked);
    }

    fn sweep(&mut self) {
        (0..self.heap.len()).for_each(|idx| {
            let object = &mut self.heap[idx];

            if object.is_marked {
                object.is_marked = false;
            } else {
                self.free(HeapId(idx));
            }
        });
    }

    pub fn mark_value(&mut self, value: Value) {
        match value {
            Value::String(heap_id) | Value::Function(heap_id) | Value::Closure(heap_id) => {
                self.mark_object(heap_id);
            }
            _ => {}
        }
    }

    pub fn mark_object(&mut self, heap_id: HeapId) {
        let object = &mut self.heap[heap_id.0];

        if object.is_marked {
            return;
        }

        object.is_marked = true;

        self.gray_stack.push_back(heap_id);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HeapId(usize);
