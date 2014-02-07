use std::hashmap::HashMap;

use error::Error;
use location::Location;
use parse1::{Def1,Parse1,Param1,Param1Ident,Param1Nil,Param1Dot};
use symbol::{Symbol,Symbols};
use token::Token;

pub struct Ast<B> {
    priv defs: ~[~[Def<B>]],
    priv names: HashMap<Symbol,(uint,uint)>,
}

#[deriving(Eq)]
pub struct DefIndex {
    priv index: uint,
}

pub struct Def<B> {
    match_bindings: ~[MatchBinding],
    expr: Expr<B>,
}

pub struct Defs<'a,B> {
    defs: &'a [Def<B>],
}

pub struct MatchBinding {
    bits: ~[bool],
    binding: Binding,
}

#[deriving(Eq)]
pub enum Binding {
    Nil, Dot, Bind
}

pub enum Expr<B> {
    ExprLiteral(Location,B),
    ExprArg(Location,uint),
    ExprFuncall(Location,DefIndex,~[Expr<B>]),
    ExprConcat(Location,~Expr<B>,~Expr<B>),
}

impl<B> Ast<B> {
    pub fn parse(file_names: &[Path], make_literal: &|~[bool]| -> B) -> Result<Ast<B>,~[Error]> {
        match Parse1::parse(&mut Token::tokenize(file_names, Symbols::new())) {
            Err(errors) => Err(errors),
            Ok(ref parse1) => Ast::parse_parse1(parse1, make_literal),
        }
    }

    #[cfg(test)]
    pub fn parse_buffer(file_name: &str, buffer: ~Buffer, make_literal: &|~[bool]| -> B) -> Result<Ast<B>,~[Error]> {
        match Parse1::parse(&mut Token::tokenize_buffer(file_name, buffer, Symbols::new())) {
            Err(errors) => Err(errors),
            Ok(ref parse1) => Ast::parse_parse1(parse1, make_literal),
        }
    }

    fn parse_parse1(parse1: &Parse1, make_literal: &|~[bool]| -> B) -> Result<Ast<B>,~[Error]> {
        let name_list = parse1.names();
        let mut names = HashMap::new();
        let mut defs = ~[];
        for i in range(0, name_list.len()) {
            names.insert(name_list[i].clone(), (i,parse1.find(&name_list[i]).unwrap()[0].params.len()));
            defs.push(~[]);
        }
        let mut errors = ~[];
        for i in range(0, name_list.len()) {
            for def1 in parse1.find(&name_list[i]).unwrap().iter() {
                match Def::parse(def1, &names, make_literal) {
                    Ok(def) => {
                        defs[i].push(def);
                    },
                    Err(error) => {
                        errors.push(error);
                    },
                }
            }
        }
        if errors.is_empty() {
            Ok(Ast { defs: defs, names: names })
        } else {
            Err(errors)
        }
    }

    fn lookup_by_index<'a>(&'a self, index: uint) -> Defs<'a,B> {
        Defs { defs: self.defs[index] }
    }

    pub fn lookup<'a>(&'a self, index: DefIndex) -> Defs<'a,B> {
        self.lookup_by_index(index.index)
    }

    #[cfg(test)]
    pub fn lookup_by_name<'a>(&'a self, name: &Symbol) -> Option<Defs<'a,B>> {
        match self.names.find(name) {
            None => None,
            Some(&(index,_)) => Some(self.lookup_by_index(index)),
        }
    }

    #[cfg(test)]
    pub fn lookup_by_str<'a>(&'a self, name: &str) -> Option<Defs<'a,B>> {
        self.lookup_by_name(&Symbols::new().intern_str(name))
    }

    pub fn lookup_index<'a>(&'a self, name: &Symbol) -> Option<DefIndex> {
        match self.names.find(name) {
            None => None,
            Some(&(index,_)) => Some(DefIndex { index:index }),
        }
    }

    pub fn lookup_index_by_str<'a>(&'a self, name: &str) -> Option<DefIndex> {
        self.lookup_index(&Symbols::new().intern_str(name))
    }
}

impl<'a,B> Defs<'a,B> {
    pub fn arity(&'a self) -> uint {
        self.defs[0].match_bindings.len()
    }
}

impl<B> Def<B> {
    fn parse(def1: &Def1, names: &HashMap<Symbol,(uint,uint)>, make_literal: &|~[bool]| -> B) -> Result<Def<B>,Error> {
        let mut match_bindings = ~[];
        let mut bindings = HashMap::new();
        for param in def1.params.iter() {
            let bind_index = bindings.len();
            match param.param_type {
                Param1Ident(ref symbol,ref location) => {
                    if bindings.contains_key(symbol) {
                        return Err(Error::new(location.clone(), format!("duplicate parameter `{}`", symbol.to_str())));
                    }
                    bindings.insert(symbol.clone(), bind_index);
                },
                _ => (),
            }
            match_bindings.push(MatchBinding::new(param));
        }
        if def1.body.is_empty() {
            Ok(Def { match_bindings: match_bindings, expr: ExprLiteral(def1.eq_location+def1.dot_location,(*make_literal)(~[])) })
        } else {
            match Expr::parse(def1.body, 0, &bindings, names, make_literal) {
                Err(error) => Err(error),
                Ok(expr) => Ok(Def { match_bindings: match_bindings, expr: expr }),
            }
        }
    }
}

impl MatchBinding {
    fn new(param: &Param1) -> MatchBinding {
        let bits = param.bits.map(|param_bit| param_bit.bit);
        let binding = match param.param_type {
            Param1Nil(_) => Nil,
            Param1Dot(_) => Dot,
            Param1Ident(_,_) => Bind,
        };
        MatchBinding { bits: bits, binding: binding }
    }
}

impl<B> Expr<B> {
    fn parse(tokens: &[Token], start_index: uint, bindings: &HashMap<Symbol,uint>, names: &HashMap<Symbol,(uint,uint)>, make_literal: &|~[bool]| -> B) -> Result<Expr<B>,Error> {
        assert!(start_index < tokens.len());
        match Expr::parse_expr(tokens, start_index, bindings, names, make_literal) {
            Err(error) => Err(error),
            Ok((next_index,expr)) => {
                if next_index >= tokens.len() {
                    Ok(expr)
                } else {
                    match Expr::parse(tokens, next_index, bindings, names, make_literal) {
                        Err(error) => Err(error),
                        Ok(next_expr) => Ok(ExprConcat(expr.location()+next_expr.location(), ~expr, ~next_expr)),
                    }
                }
            }
        }
    }

    fn parse_expr(tokens: &[Token], start_index: uint, bindings: &HashMap<Symbol,uint>, names: &HashMap<Symbol,(uint,uint)>, make_literal: &|~[bool]| -> B) -> Result<(uint,Expr<B>),Error> {
        assert!(start_index < tokens.len());
        let tok = &tokens[start_index];
        if tok.is_zero() || tok.is_one() || tok.is_nil() {
            return Ok(Expr::parse_literal(tokens, start_index, make_literal));
        }
        assert!(tok.is_ident());
        match bindings.find(&tok.ident().unwrap()) {
            None => (),
            Some(arg_index) => {
                return Ok((start_index+1, ExprArg(tok.location().clone(), *arg_index)));
            },
        }
        let (def_index,arity) = match names.find(&tok.ident().unwrap()) {
            None => {
                return Err(Error::new(tok.location().clone(), format!("unknown identifier `{}`", tok.to_str())));
            },
            Some(def_index_arity) => *def_index_arity,
        };
        let mut expr_index = start_index + 1;
        let mut args = ~[];
        let mut location = tok.location().clone();
        for i in range(0,arity) {
            if expr_index >= tokens.len() {
                return Err(Error::new(tok.location().clone() + *tokens.last().unwrap().location(), format!("`{}` given {} argument(s), requires {} argument(s)", tok.to_str(), i, arity)));
            }
            match Expr::parse_expr(tokens, expr_index, bindings, names, make_literal) {
                Err(error) => { return Err(error); },
                Ok((next_index,expr)) => {
                    expr_index = next_index;
                    location = location + expr.location();
                    args.push(expr);
                },
            }
        }
        assert!(arity == args.len());
        Ok((expr_index,ExprFuncall(location,DefIndex { index:def_index },args)))
    }

    fn parse_literal(tokens: &[Token], start_index: uint, make_literal: &|~[bool]| -> B) -> (uint,Expr<B>) {
        let mut bits = ~[];
        let mut location = tokens[start_index].location().clone();
        for i in range(start_index, tokens.len()) {
            if tokens[i].is_zero() {
                location = location + *tokens[i].location();
                bits.push(false);
            } else if tokens[i].is_one() {
                location = location + *tokens[i].location();
                bits.push(true);
            } else if tokens[i].is_nil() {
                location = location + *tokens[i].location();
                return (i+1,ExprLiteral(location,(*make_literal)(bits)));
            } else {
                return (i,ExprLiteral(location,(*make_literal)(bits)));
            }
        }
        (tokens.len(),ExprLiteral(location,(*make_literal)(bits)))
    }

    pub fn location(&self) -> Location {
        match *self {
            ExprLiteral(ref location,_) => location.clone(),
            ExprArg(ref location,_) => location.clone(),
            ExprFuncall(ref location,_,_) => location.clone(),
            ExprConcat(ref location,_,_) => location.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::{Ast,Bind,Dot,Nil,ExprLiteral,ExprArg,ExprFuncall,ExprConcat};
    use bits::Bits;
    use bits1::Bits1;
    use error::Error;

    fn parse(src: &str) -> Result<Ast<Bits1>,~[Error]> {
        use std::io::{BufferedReader,MemReader};
        Ast::parse_buffer("-", ~BufferedReader::new(MemReader::new(src.as_bytes().to_owned())), &|bits| Bits::from_vec(bits))
    }

    #[test]
    fn test_ast() {
        let ast = parse("cat a=a.").unwrap();
        assert!(ast.lookup_by_str("a").is_none());
        let defs = ast.lookup_by_str("cat").unwrap();
        assert!(defs.arity() == 1);
        assert!(defs.defs.len() == 1);
        assert!([] == defs.defs[0].match_bindings[0].bits);
        assert!(defs.defs[0].match_bindings[0].binding == Bind);
        match defs.defs[0].expr {
            ExprArg(_,index) => { assert!(index == 0); },
            _ => { fail!("defs.defs[0].expr"); },
        }

        let ast = parse("cat a=cat.").unwrap_err();
        assert!(ast.len() == 1);
        assert!("-:1:7: 1:9: `cat` given 0 argument(s), requires 1 argument(s)" == ast[0].to_str());

        let ast = parse("cat a=b.").unwrap_err();
        assert!(ast.len() == 1);
        assert!("-:1:7: 1:7: unknown identifier `b`" == ast[0].to_str());

        let ast = parse("ones=1ones.zeros=0zeros.").unwrap();
        let defs = ast.lookup_by_str("ones").unwrap();
        assert!(defs.arity() == 0);
        assert!(defs.defs.len() == 1);
        match defs.defs[0].expr {
            ExprConcat(_,~ExprLiteral(_,ref literal),~ExprFuncall(_,ref index,ref args)) => {
                assert!("1" == literal.to_str());
                assert!(*index == ast.lookup_index_by_str("ones").unwrap());
                assert!(args.is_empty());
            },
            _ => { fail!("defs.defs[0].expr"); },
        }
        let defs = ast.lookup_by_str("zeros").unwrap();
        assert!(defs.arity() == 0);
        assert!(defs.defs.len() == 1);
        match defs.defs[0].expr {
            ExprConcat(_,~ExprLiteral(_,ref literal),~ExprFuncall(_,ref index,ref args)) => {
                assert!("0" == literal.to_str());
                assert!(*index == ast.lookup_index_by_str("zeros").unwrap());
                assert!(args.is_empty());
            },
            _ => { fail!("defs.defs[0].expr"); },
        }

        let ast = parse("a_0=.a.2=b b 2.b.=.").unwrap();
        let defs = ast.lookup_by_str("a").unwrap();
        assert!(defs.arity() == 2);
        assert!(defs.defs.len() == 2);
        assert!([] == defs.defs[0].match_bindings[0].bits);
        assert!(defs.defs[0].match_bindings[0].binding == Nil);
        assert!([false] == defs.defs[0].match_bindings[1].bits);
        assert!(defs.defs[0].match_bindings[1].binding == Dot);
        match defs.defs[0].expr {
            ExprLiteral(_,ref bits) => { assert!("" == bits.to_str()); },
            _ => { fail!("defs.defs[0].expr"); },
        }
        assert!([] == defs.defs[1].match_bindings[0].bits);
        assert!(defs.defs[1].match_bindings[0].binding == Dot);
        assert!([] == defs.defs[1].match_bindings[1].bits);
        assert!(defs.defs[1].match_bindings[1].binding == Bind);
        let b_index = match defs.defs[1].expr {
            ExprFuncall(_,ref index1,[ExprFuncall(_,ref index2,[ExprArg(_,0)])]) => {
                assert!(*index1 == *index2);
                *index1
            },
            _ => { fail!("defs.defs[0].expr") },
        };
        let defs = ast.lookup(b_index);
        assert!(defs.arity() == 1);
        assert!(defs.defs.len() == 1);
        assert!([] == defs.defs[0].match_bindings[0].bits);
        assert!(defs.defs[0].match_bindings[0].binding == Dot);
        match defs.defs[0].expr {
            ExprLiteral(_,ref bits) => { assert!("" == bits.to_str()); },
            _ => { fail!("defs.defs[0].expr"); },
        }
    }

    #[test]
    fn test_parse_files() {
        use std::io::fs;
        use std::libc;
        use std::os;
        use std::io::File;
        let path = os::tmpdir().join(format!("test{}", unsafe { libc::getpid() }));
        File::create(&path).write(bytes!("x=x.y z=z.")).ok();
        let ast : Ast<Bits1> = Ast::parse([path.clone()], &|bits| Bits::from_vec(bits)).unwrap();
        fs::unlink(&path).ok();
        let defs = ast.lookup_by_str("x").unwrap();
        assert!(defs.arity() == 0);
        assert!(defs.defs.len() == 1);
        let defs = ast.lookup_by_str("y").unwrap();
        assert!(defs.arity() == 1);
        assert!(defs.defs.len() == 1);
    }
}
