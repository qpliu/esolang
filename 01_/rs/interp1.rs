use std::rc::Rc;

use ast::{Ast,Bind,Def,DefIndex,Expr,ExprLiteral,ExprArg,ExprFuncall,ExprConcat,Nil};
use bits::Bits;
use bits1::{Bits1,LazyListIterator};
use interp::Interp;
use location::Location;

pub struct Interp1;

impl Interp<Bits1> for Interp1 {
    fn new() -> Interp1 { Interp1 }

    fn nil(&mut self) -> Bits1 { Bits::nil() }

    fn constant(&mut self, bits: ~[bool]) -> Bits1 { Bits::from_vec(bits) }

    fn file(&mut self, path: &Path) -> Bits1 { Bits::from_file(path) }

    fn reader(&mut self, reader: ~Reader) -> Bits1 { Bits::from_reader(reader) }

    fn run(self, ast: Ast<Bits1>, f: DefIndex, args: ~[Bits1], writer: &mut Writer) {
        eval_funcall(&Rc::new(ast), f, args, None).write(writer);
    }
}

fn eval_funcall(ast: &Rc<Ast<Bits1>>, f: DefIndex, args: ~[Bits1], location: Option<Location>) -> Bits1 {
    Bits1::new(~Unevaluated(Funcall { ast:ast.clone(), f:f, args:args, location:location }))
}

struct Funcall {
    ast: Rc<Ast<Bits1>>,
    f: DefIndex,
    args: ~[Bits1],
    location: Option<Location>,
}

enum FuncallBitIterator {
    Unevaluated(Funcall),
    Evaluated(LazyListIterator<bool>),
}

impl Iterator<bool> for FuncallBitIterator {
    fn next(&mut self) -> Option<bool> {
        let mut result = None;
        let mut replacement = None;
        match *self {
            Evaluated(ref mut iter) => { result = iter.next(); }
            Unevaluated(Funcall { ast:ref ast, f:ref f, args:ref args, location:ref location }) => {
                let defs = ast.borrow().lookup(*f);
                assert!(defs.arity() == args.len());
                for def in defs.defs.iter() {
                    match match_bindings(def, *args) {
                        None => (),
                        Some(bindings) => {
                            let mut iter = eval_expr(ast, &def.expr, bindings).iter();
                            result = iter.next();
                            replacement = Some(Evaluated(iter));
                            break;
                        },
                    }
                }
                if replacement.is_none() {
                    match *location {
                        None => fail!("Failed to match"),
                        Some(ref loc) => fail!(format!("{}: Failed to match", loc.to_str())),
                    }
                }
            }
        }
        match replacement {
            None => (),
            Some(item) => { *self = item; }
        }
        result
    }
}

fn match_bindings(def: &Def<Bits1>, args: &[Bits1]) -> Option<~[Bits1]> {
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
            Bind => { bindings.push(arg); },
            Nil if !arg.eval().is_none() => { return None; },
            _ => (),
        }
    }
    Some(bindings)
}

fn eval_expr(ast: &Rc<Ast<Bits1>>, expr: &Expr<Bits1>, bindings: &[Bits1]) -> Bits1 {
    match *expr {
        ExprLiteral(_,ref bits) => bits.clone(),
        ExprArg(_,index) => bindings[index].clone(),
        ExprConcat(_,ref expr1,ref expr2) => {
            let bits1 = eval_expr(ast, *expr1, bindings);
            let bits2 = eval_expr(ast, *expr2, bindings);
            bits1 + bits2
        },
        ExprFuncall(ref location,ref f,ref exprs) => {
            let args = exprs.map(|e| eval_expr(ast, e, bindings));
            eval_funcall(ast, *f, args, Some(location.clone()))
        },
    }
}

#[cfg(test)]
mod tests {
    use ast::Ast;
    use bits::Bits;
    use bits1::Bits1;

    fn run(src: &str, f: &str, args: ~[Bits1]) -> Bits1 {
        use std::io::MemReader;
        use std::rc::Rc;
        let ast = Ast::parse_buffer("-", ~MemReader::new(src.as_bytes().to_owned()), &|bits| Bits::from_vec(bits)).unwrap();
        let f_index = ast.lookup_index_by_str(f).unwrap();
        super::eval_funcall(&Rc::new(ast), f_index, args, None)
    }

    #[test]
    fn test_eval() {
        let b10 = run("f=10.", "f", ~[]);
        assert!("10" == b10.to_str());

        let b1010 = run("id x=x.", "id", ~[b10+b10]);
        assert!("1010" == b1010.to_str());

        let b0101 = run("-0b=1- b.-1b=0- b.-_=_.", "-", ~[b1010.clone()]);
        assert!("0101" == b0101.to_str());
    }

    #[test]
    #[should_fail]
    fn test_fail() {
        run("f=g1.g0=.","f",~[]).to_str();
    }

    #[test]
    fn test_lazy() {
        run("f=g1.g0=.","f",~[]);
    }
}
