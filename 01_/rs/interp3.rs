use std::cell::RefCell;
use std::rc::Rc;
use ast;
use ast::{Ast,Def,DefIndex,Expr};
use bits::Bits;
use interp::Interp;

pub struct Interp3;

pub struct Bits3 {
    priv data: Rc<RefCell<Data>>,
}

enum Data {
    Nil,
    Val(bool,Bits3),
    FromReader{bit: u8, byte: u8, reader: ~Reader},
    Concat(Bits3,Bits3),
    Funcall{ast: *Ast<Bits3>, def_index: DefIndex, args: ~[Bits3]},
    ExprData{ast: *Ast<Bits3>, expr: *Expr<Bits3>, bindings: ~[Bits3]},
    Trampoline(Bits3),
}

enum EvalResult {
    Done(Option<(bool,Bits3)>),
    Insert(bool),
    Replace(Data),
}

impl Interp<Bits3> for Interp3 {
    fn new() -> Interp3 {
        Interp3
    }

    fn nil(&mut self) -> Bits3 {
        Bits::nil()
    }

    fn constant(&mut self, bits: ~[bool]) -> Bits3 {
        Bits::from_vec(bits)
    }

    fn file(&mut self, file: &Path) -> Bits3 {
        Bits::from_file(file)
    }

    fn reader(&mut self, reader: ~Reader) -> Bits3 {
        Bits::from_reader(reader)
    }

    fn run(self, ast: Ast<Bits3>, def_index: DefIndex, args: ~[Bits3], writer: &mut Writer) {
        Bits3::from_data(Funcall{ast:&ast, def_index:def_index, args:args}).write(writer);
    }
}

impl Bits3 {
    fn from_data(data: Data) -> Bits3 {
        Bits3 { data: Rc::new(RefCell::new(data)) }
    }

    fn partial_eval(&mut self) -> EvalResult {
        let cell = self.data.borrow();
        let mut ref_mut = cell.borrow_mut();
        let val = ref_mut.get();
        match *val {
            Nil => Done(None),
            Val(b,ref rest) => Done(Some((b,rest.clone()))),
            FromReader{ref mut bit, ref mut byte, ref mut reader} => {
                if *bit != 0 {
                    *bit >>= 1;
                    Insert(*bit & *byte != 0)
                } else {
                    match reader.read_byte() {
                        None => Replace(Nil),
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
                            None => Replace(Nil),
                            Some((b,rest)) => Replace(Val(b,rest)),
                        }
                    }
                }
            },
            Funcall{ast: ref ast, def_index: ref def_index, args: ref args} =>
                Replace(force_funcall(*ast, *def_index, *args)),
            ExprData{ast: ref ast, expr: ref expr, bindings: ref bindings} =>
                Replace(force_expr(*ast, *expr, *bindings)),
            Trampoline(ref mut bits) => {
                match bits.eval() {
                    None => Replace(Nil),
                    Some((b,rest)) => Replace(Val(b,rest)),
                }
            },
        }
    }
}

impl Bits for Bits3 {
    fn from_vec(vec: &[bool]) -> Bits3 {
        let mut result = Bits::nil();
        for &b in vec.rev_iter() {
            result = Bits3::from_data(Val(b,result));
        }
        result
    }

    fn from_reader(reader: ~Reader) -> Bits3 {
        Bits3::from_data(FromReader{bit: 0, byte: 0, reader: reader})
    }

    fn nil() -> Bits3 {
        Bits3::from_data(Nil)
    }

    fn eval(&mut self) -> Option<(bool,Bits3)> {
        use std::util::replace;
        loop {
            let eval_result = self.partial_eval();
            let cell = self.data.borrow();
            let mut ref_mut = cell.borrow_mut();
            let val = ref_mut.get();
            match eval_result {
                Done(result) => { return result; },
                Insert(b) => {
                    let next_val = replace(val,Nil);
                    let rest = Bits3::from_data(next_val);
                    *val = Val(b,rest.clone());
                    return Some((b,rest));
                },
                Replace(new_val) => {
                    *val = new_val;
                },
            }
        }
    }
}

impl Clone for Bits3 {
    fn clone(&self) -> Bits3 {
        Bits3 { data: self.data.clone() }
    }
}

fn force_funcall(ast: *Ast<Bits3>, def_index: DefIndex, args: &[Bits3]) -> Data {
    let defs = unsafe { &*ast }.lookup(def_index);
    assert!(defs.arity() == args.len());
    for def in defs.defs.iter() {
        match match_bindings(def, args) {
            None => (),
            Some(bindings) => {
                return ExprData{ast: ast.clone(), expr: &def.expr, bindings: bindings};
            }
        }
    }
    fail!("Failed to match")
}

fn match_bindings(def: &Def<Bits3>, args: &[Bits3]) -> Option<~[Bits3]> {
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

fn force_expr(ast: *Ast<Bits3>, expr: *Expr<Bits3>, bindings: &[Bits3]) -> Data {
    unsafe {
        match *expr {
            ast::ExprLiteral(_,ref bits) => Trampoline(bits.clone()),
            ast::ExprArg(_,index) => Trampoline(bindings[index].clone()),
            ast::ExprConcat(_,ref expr1,ref expr2) => {
                let bits1 = Bits3::from_data(force_expr(ast, &**expr1, bindings));
                let bits2 = Bits3::from_data(force_expr(ast, &**expr2, bindings));
                Concat(bits1, bits2)
            },
            ast::ExprFuncall(_,ref f,ref exprs) => {
                let args = exprs.map(|e| Bits3::from_data(force_expr(ast, e, bindings)));
                Funcall{ast:ast, def_index:*f, args:args}
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::Ast;
    use bits::Bits;
    use interp3::Bits3;

    fn run(src: &str, f: &str, args: ~[Bits3]) -> (~Ast<Bits3>,Bits3) {
        use std::io::MemReader;
        let ast = ~Ast::parse_buffer("-", ~MemReader::new(src.as_bytes().to_owned()), &|bits| Bits::from_vec(bits)).unwrap();
        let f_index = ast.lookup_index_by_str(f).unwrap();
        let bits = Bits3::from_data(super::force_funcall(&*ast, f_index, args));
        (ast,bits)
    }

    #[test]
    fn test_eval() {
        let (_ast,b10) = run("f=10.", "f", ~[]);
        assert!("10" == b10.bits_to_str());

        let (_ast,b1010) = run("2 x=x x.", "2", ~[b10]);
        assert!("1010" == b1010.bits_to_str());

        let (_ast,b0101) = run("-0b=1- b.-1b=0- b.-_=_.", "-", ~[b1010.clone()]);
        assert!("0101" == b0101.bits_to_str());
    }

    #[test]
    #[should_fail]
    fn test_fail() {
        let (_ast,b) = run("f=g1.g0=.","f",~[]);
        b.bits_to_str();
    }

    #[test]
    fn test_lazy() {
        let _ = run("f=g1.g0=.","f",~[]);
    }
}
