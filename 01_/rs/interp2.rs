use std::cell::RefCell;
use std::rc::Rc;
use ast::{Ast,DefIndex};
use bits::Bits;
use interp::Interp;

pub struct Interp2;

pub struct Bits2 {
    priv data: Rc<RefCell<Data>>,
}

enum Data {
    Nil,
    Val(bool,Bits2),
    FromReader{bit: u8, byte: u8, reader: ~Reader},
    Concat(Bits2,Bits2),
    Funcall{ast: Rc<Ast<Bits2>>, def_index: DefIndex, args: ~[Bits2]},
}

impl Interp<Bits2> for Interp2 {
    fn new() -> Interp2 {
        Interp2
    }

    fn nil(&mut self) -> Bits2 {
        Bits::nil()
    }

    fn constant(&mut self, bits: ~[bool]) -> Bits2 {
        Bits::from_vec(bits)
    }

    fn file(&mut self, file: &Path) -> Bits2 {
        Bits::from_file(file)
    }

    fn reader(&mut self, reader: ~Reader) -> Bits2 {
        Bits::from_reader(reader)
    }

    fn run(self, ast: Ast<Bits2>, def_index: DefIndex, args: ~[Bits2], writer: &mut Writer) {
        Bits2::from_funcall(Rc::new(ast), def_index, args).write(writer);
    }
}

impl Bits2 {
    fn from_funcall(ast: Rc<Ast<Bits2>>, def_index: DefIndex, args: ~[Bits2]) -> Bits2 {
        Bits2 { data: Rc::new(RefCell::new(Funcall{ast: ast, def_index: def_index, args: args})) }
    }
}

impl Bits for Bits2 {
    fn from_vec(vec: &[bool]) -> Bits2 {
        let mut result = Bits::nil();
        for &b in vec.rev_iter() {
            result = Bits2 { data: Rc::new(RefCell::new(Val(b, result))) };
        }
        result
    }

    fn from_reader(reader: ~Reader) -> Bits2 {
        Bits2 { data: Rc::new(RefCell::new(FromReader{bit: 0, byte: 0, reader: reader})) }
    }

    fn nil() -> Bits2 {
        Bits2 { data: Rc::new(RefCell::new(Nil)) }
    }

    fn eval(&self) -> Option<(bool,Bits2)> {
        fail!()
    }
}

impl Clone for Bits2 {
    fn clone(&self) -> Bits2 {
        Bits2 { data: self.data.clone() }
    }
}

#[cfg(test)]
mod tests {
}
