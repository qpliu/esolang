use std::cell::RefCell;
use std::rc::Rc;
use ast;
use ast::{Ast,Def,DefIndex,Expr};
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
                    match reader.read_byte().ok() {
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
            Funcall{ast: ref ast, def_index: ref def_index, args: ref args} => {
                match force_funcall(ast, *def_index, *args).eval() {
                    None => End,
                    Some((b,rest)) => Replace(b,rest),
                }
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

fn force_funcall(ast: &Rc<Ast<Bits2>>, def_index: DefIndex, args: &[Bits2]) -> Bits2 {
    let defs = ast.borrow().lookup(def_index);
    assert!(defs.arity() == args.len());
    for def in defs.defs.iter() {
        match match_bindings(def, args) {
            None => (),
            Some(bindings) => {
                return eval_expr(ast, &def.expr, bindings);
            }
        }
    }
    fail!("Failed to match")
}

fn match_bindings(def: &Def<Bits2>, args: &[Bits2]) -> Option<~[Bits2]> {
    assert!(def.match_bindings.len() == args.len());
    let mut bindings = ~[];
    for i in range(0, args.len()) {
        let mut arg = args[i].clone();
        for &b in def.match_bindings[i].bits.iter() {
            match arg.eval() {
                None => { return None; },
                Some((bit,_)) if bit != b => { return None; },
                Some((_,tail)) => { arg = tail.clone(); },
            }
        }
        match def.match_bindings[i].binding {
            ast::Bind => { bindings.push(arg); },
            ast::Nil if !arg.eval().is_none() => { return None; },
            _ => (),
        }
    }
    Some(bindings)
}

fn eval_expr(ast: &Rc<Ast<Bits2>>, expr: &Expr<Bits2>, bindings: &[Bits2]) -> Bits2 {
    match *expr {
        ast::ExprLiteral(_,ref bits) => bits.clone(),
        ast::ExprArg(_,index) => bindings[index].clone(),
        ast::ExprConcat(_,ref expr1,ref expr2) => {
            let bits1 = eval_expr(ast, *expr1, bindings);
            let bits2 = eval_expr(ast, *expr2, bindings);
            Bits2::from_data(Concat(bits1, bits2))
        },
        ast::ExprFuncall(_,ref f,ref exprs) => {
            let args = exprs.map(|e| eval_expr(ast, e, bindings));
            Bits2::from_data(Funcall{ast:ast.clone(), def_index:*f, args:args})
        },
    }
}

#[cfg(test)]
mod tests {
    use ast::Ast;
    use bits::Bits;
    use interp2::Bits2;

    fn run(src: &str, f: &str, args: ~[Bits2]) -> Bits2 {
        use std::io::{BufferedReader,MemReader};
        use std::rc::Rc;
        let ast : Ast<Bits2> = Ast::parse_buffer("-", ~BufferedReader::new(MemReader::new(src.as_bytes().to_owned())), &|bits| Bits::from_vec(bits)).unwrap();
        let f_index = ast.lookup_index_by_str(f).unwrap();
        super::force_funcall(&Rc::new(ast), f_index, args)
    }

    #[test]
    fn test_eval() {
        let b10 = run("f=10.", "f", ~[]);
        assert!("10" == b10.bits_to_str());

        let b1010 = run("2 x=x x.", "2", ~[b10]);
        assert!("1010" == b1010.bits_to_str());

        let b0101 = run("-0b=1- b.-1b=0- b.-_=_.", "-", ~[b1010.clone()]);
        assert!("0101" == b0101.bits_to_str());
    }

    #[test]
    #[should_fail]
    fn test_fail() {
        run("f=g1.g0=.","f",~[]).bits_to_str();
    }

    #[test]
    fn test_lazy() {
        run("f=g1.g0=.","f",~[]);
    }
}
