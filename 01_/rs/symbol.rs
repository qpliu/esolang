use std::hashmap::HashMap;
use std::to_bytes::{Cb,IterBytes};
use std::cell::RefCell;
use std::rc::Rc;
use std::str;

pub struct Symbol {
    priv name: Rc<~str>,
}

pub struct Symbols {
    priv map: Rc<RefCell<HashMap<~str,Symbol>>>,
}

impl Clone for Symbol {
    fn clone(&self) -> Symbol {
        Symbol { name: self.name.clone() }
    }
}

impl Eq for Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.name == other.name
    }
}

impl IterBytes for Symbol {
    fn iter_bytes(&self, lsb0: bool, f: Cb) -> bool {
        self.name.iter_bytes(lsb0, f)
    }
}

impl ToStr for Symbol {
    fn to_str(&self) -> ~str {
        self.name.borrow().to_str()
    }
}

impl Symbols {
    pub fn new() -> Symbols {
        Symbols { map: Rc::new(RefCell::new(HashMap::new())) }
    }

    pub fn intern_str(&self, name: &str) -> Symbol {
        let mut refmut = self.map.borrow().borrow_mut();
        refmut.get().find_or_insert_with(name.to_owned(), |k| -> Symbol {
                Symbol { name: Rc::new(k.to_owned()) }
            }).clone()
    }

    pub fn intern(&self, name: &[char]) -> Symbol {
        self.intern_str(str::from_chars(name))
    }
}

impl Clone for Symbols {
    fn clone(&self) -> Symbols {
        Symbols { map: self.map.clone() }
    }
}

#[cfg(test)]
mod tests {
    use super::Symbols;

    #[test]
    fn test() {
        let symbols = Symbols::new();
        assert!(~"a" == symbols.intern(['a']).to_str());
        assert!(~"abc" == symbols.intern_str("abc").to_str());
    }
}
