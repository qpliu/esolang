use std::rc::Rc;

use ast::{Ast,Bind,Def,DefIndex,Expr,ExprLiteral,ExprArg,ExprFuncall,ExprConcat,Nil};
use bits::{Bits,LazyListIterator};
use location::Location;

pub fn eval(ast: &Rc<Ast>, f: DefIndex, args: ~[Bits]) -> Bits {
    eval_funcall(ast, f, args, None)
}

fn eval_funcall(ast: &Rc<Ast>, f: DefIndex, args: ~[Bits], location: Option<Location>) -> Bits {
    Bits::new(~Unevaluated(Funcall { ast:ast.clone(), f:f, args:args, location:location }))
}

struct Funcall {
    ast: Rc<Ast>,
    f: DefIndex,
    args: ~[Bits],
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

fn match_bindings(def: &Def, args: &[Bits]) -> Option<~[Bits]> {
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

fn eval_expr(ast: &Rc<Ast>, expr: &Expr, bindings: &[Bits]) -> Bits {
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
    use std::rc::Rc;
    use interp;
    use ast::Ast;

    fn parse(src: &str) -> Ast {
        use std::io::mem::MemReader;
        Ast::parse_buffer("-", ~MemReader::new(src.as_bytes().to_owned())).unwrap()
    }

    #[test]
    fn test_eval() {
        let ast = parse("f=10.");
        let f = ast.lookup_index_by_str("f").unwrap();
        let b10 = interp::eval(&Rc::new(ast), f, ~[]);
        assert!("10" == b10.to_str());

        let ast = parse("id x=x.");
        let f = ast.lookup_index_by_str("id").unwrap();
        let b1010 = interp::eval(&Rc::new(ast), f, ~[b10+b10]);
        assert!("1010" == b1010.to_str());

        let ast = parse("-0b=1- b.-1b=0- b.-_=_.");
        let f = ast.lookup_index_by_str("-").unwrap();
        let b0101 = interp::eval(&Rc::new(ast), f, ~[b1010.clone()]);
        assert!("0101" == b0101.to_str());
    }

    #[test]
    #[should_fail]
    fn test_fail() {
        let ast = parse("f=g1.g0=.");
        let f = ast.lookup_index_by_str("f").unwrap();
        interp::eval(&Rc::new(ast), f, ~[]).to_str();
    }

    #[test]
    fn test_lazy() {
        let ast = parse("f=g1.g0=.");
        let f = ast.lookup_index_by_str("f").unwrap();
        interp::eval(&Rc::new(ast), f, ~[]);
    }
}
