use std::collections::HashMap;
use typed_arena::Arena;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StrId(usize);

pub(crate) struct Interner<'a> {
    map: HashMap<&'a str, StrId>,
    vec: Vec<&'a str>,
    arena: &'a Arena<u8>,
}

impl Interner<'_> {
    pub(crate) fn new(arena: &Arena<u8>) -> Interner {
        Interner {
            map: HashMap::new(),
            vec: Vec::new(),
            arena,
        }
    }

    pub(crate) fn intern(&mut self, name: &str) -> StrId {
        if let Some(&idx) = self.map.get(name) {
            return idx;
        }
        let idx = self.vec.len();
        let str_id = StrId(idx);
        let name = self.arena.alloc_str(name);
        self.map.insert(name, str_id);
        self.vec.push(name);

        debug_assert!(self.lookup(str_id) == name);
        debug_assert!(self.intern(name) == str_id);

        str_id
    }

    pub(crate) fn lookup(&self, idx: StrId) -> &str {
        self.vec[idx.0]
    }
}
