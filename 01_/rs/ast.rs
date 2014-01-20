use std::hashmap::HashMap;

use error::Error;
use location::Location;
use parse1::Parse1;
use symbol::Symbol;

pub struct Ast {
    priv defs: ~[~[Def]],
    priv mainIndex: uint,
}

pub struct DefIndex {
    priv index: uint,
}

pub struct Def {
    matchBindings: ~[MatchBinding],
    expr: Expr,
}

pub struct Defs<'a> {
    defs: &'a [Def],
}

pub struct MatchBinding {
    bits: ~[bool],
    binding: Binding,
}

pub enum Binding {
    Nil, Dot, Bind
}

pub enum Expr {
    ExprConst(~[bool]),
    ExprArg(uint),
    ExprFuncall(Location,DefIndex,~[Expr]),
    ExprConcat(~Expr,~Expr),
}

impl Ast {
    pub fn parse(parse1: &Parse1, main: &Symbol) -> Result<Ast,~[Error]> {
        fail!()
    }
}

impl<'a> Ast {
    pub fn main(&'a self) -> Defs<'a> {
        Defs { defs: self.defs[self.mainIndex] }
    }

    pub fn lookup(&'a self, index: DefIndex) -> Defs<'a> {
        Defs { defs: self.defs[index.index] }
    }
}

impl MatchBinding {
    pub fn is_nil(&self) -> bool {
        match self.binding {
            Nil => true,
            _ => false,
        }
    }

    pub fn is_dot(&self) -> bool {
        match self.binding {
            Dot => true,
            _ => false,
        }
    }

    pub fn is_bind(&self) -> bool {
        match self.binding {
            Bind => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ast() {
    }
}
