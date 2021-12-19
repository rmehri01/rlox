use crate::object::{ObjData, Object};
use rustc_hash::FxHashMap;

// TODO: try arena
pub struct Memory {
    map: FxHashMap<String, HeapId>,
    objects: Vec<Object>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            map: FxHashMap::default(),
            objects: Vec::new(),
        }
    }

    pub fn alloc(&mut self, data: ObjData) -> HeapId {
        // println!("allocating {:?}", data);

        // ...
        self.collect_garbage();
        // ...

        self.objects.push(Object::new(data));
        HeapId(self.objects.len() - 1)
    }

    pub fn lookup(&self, heap_id: HeapId) -> &ObjData {
        &self.objects[heap_id.0].data
    }

    pub fn intern(&mut self, name: &str) -> HeapId {
        if let Some(&idx) = self.map.get(name) {
            return idx;
        }

        let str_id = self.alloc(ObjData::String(name.to_string()));
        self.map.insert(name.to_string(), str_id);

        str_id
    }

    fn free(&self) {
        // println!("freeing {:?}", todo!());
    }

    fn collect_garbage(&self) {
        // println!("-- gc begin");

        // println!("-- gc end")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HeapId(usize);
