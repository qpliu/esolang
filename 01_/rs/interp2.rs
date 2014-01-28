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
        Bits2::from_data(Funcall{ast:Rc::new(ast), def_index:def_index, args:args}).write(writer);
    }
}

impl Bits2 {
    fn from_data(data: Data) -> Bits2 {
        Bits2 { data: Rc::new(RefCell::new(data)) }
    }
}

impl Bits for Bits2 {
    fn from_vec(vec: &[bool]) -> Bits2 {
        let mut result = Bits::nil();
        for &b in vec.rev_iter() {
            result = Bits2::from_data(Val(b,result));
        }
        result
    }

    fn from_reader(reader: ~Reader) -> Bits2 {
        Bits2::from_data(FromReader{bit: 0, byte: 0, reader: reader})
    }

    fn nil() -> Bits2 {
        Bits2::from_data(Nil)
    }

    fn eval(&mut self) -> Option<(bool,Bits2)> {
        use std::util::replace;

        enum EvalResult {
            End,
            Insert(bool),
            Replace(bool,Bits2),
        }

        let cell = self.data.borrow();
        let mut ref_mut = cell.borrow_mut();
        let val = ref_mut.get();
        match match *val {
            Nil => { return None },
            Val(b,ref rest) => { return Some((b,rest.clone())) },
            FromReader{ref mut bit, ref mut byte, ref mut reader} => {
                if *bit != 0 {
                    *bit >>= 1;
                    Insert(*bit & *byte != 0)
                } else {
                    match reader.read_byte() {
                        None => End,
                        Some(next_byte) => {
                            *bit = 64;
                            *byte = next_byte;
                            Insert(next_byte & 128 != 0)
                        }
                    }
                }
            },
            Concat(ref mut first,ref mut last) => {
                match first.eval() {
                    Some((b,first_rest)) => {
                        *first = first_rest;
                        Insert(b)
                    },
                    None => {
                        match last.eval() {
                            None => End,
                            Some((b,rest)) => Replace(b,rest),
                        }
                    }
                }
            },
            Funcall{ast: ref _ast, def_index: ref _def_index, args: ref _args} => {
                fail!()
            },
        } {
            End => {
                *val = Nil;
                None
            },
            Insert(b) => {
                let next_val = replace(val, Nil);
                let rest = Bits2::from_data(next_val);
                *val = Val(b,rest.clone());
                Some((b,rest))
            },
            Replace(b,rest) => {
                *val = Val(b,rest.clone());
                Some((b,rest))
            },
        }
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
