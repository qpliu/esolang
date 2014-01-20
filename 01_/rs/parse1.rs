use std::hashmap::HashMap;

use error::Error;
use location::Location;
use symbol::Symbol;
use token::{Zero,One,Nil,Equal,Dot,Ident,Token};

pub struct Parse1 {
    priv map: HashMap<Symbol,~[Def1]>,
}

pub struct Def1 {
    ident: Symbol,
    ident_location: Location,
    params: ~[Param1],
    eq_location: Location,
    body: ~[Token],
    dot_location: Location,
}

pub struct Param1 {
    bits: ~[ParamBit1],
    param_type: ParamType1,
}

pub struct ParamBit1 {
    bit: bool,
    location: Location,
}

pub enum ParamType1 {
    Param1Dot(Option<Location>),
    Param1Nil(Location),
    Param1Ident(Symbol,Location),
}

impl Container for Parse1 {
    fn len(&self) -> uint {
        self.map.len()
    }
}

impl<'a> Map<Symbol,~[Def1]> for Parse1 {
    fn find<'b>(&'b self, key: &Symbol) -> Option<&'b ~[Def1]> {
        self.map.find(key)
    }
}

impl Parse1 {
    pub fn parse(tokens: &mut Iterator<Token>) -> Result<Parse1,~[Error]> {
        let mut map = HashMap::new();
        loop {
            let res = Def1::parse(tokens);
            match res {
                None => { return Parse1::verify(map) },
                Some(Err(err)) => { return Err(~[err]) },
                _ => (),
            }
            let def1 = res.unwrap().unwrap();
            map.mangle(def1.ident.clone(), def1, |_,d| ~[d], |_,ds,d| ds.push(d));
        }
    }

    fn verify(map: HashMap<Symbol,~[Def1]>) -> Result<Parse1,~[Error]> {
        let mut errors = ~[];
        for (name, defs) in map.iter() {
            assert!(defs.len() >= 1);
            let def0 = &defs[0];
            let mut has_error = false;
            for def in defs.iter() {
                if def.params.len() != def0.params.len() {
                    has_error = true;
                    errors.push(Error::new(def.head_location(), format!("`{}` defined with {} argument(s), originally defined with {} argument(s)", name.to_str(), def.params.len(), def0.params.len())));
                }
            }
            if has_error {
                errors.push(Error::new(def0.head_location(), format!("`{}` originally defined with {} argument(s)", name.to_str(), def0.params.len())));
            }
        }
        if errors.is_empty() {
            Ok(Parse1 { map: map })
        } else {
            Err(errors)
        }
    }

    pub fn names(&self) -> ~[Symbol] {
        let keys = self.map.keys();
        let mut mkeys = keys.map(|s| s.clone());
        mkeys.collect()
    }
}

impl Def1 {
    fn parse(tokens: &mut Iterator<Token>) -> Option<Result<Def1,Error>> {
        match tokens.next() {
            None => { return None },
            Some(Ident(ident,ident_location)) => {
                let mut params = ~[];
                let mut option_eq_location;
                loop {
                    match Param1::parse(tokens, &ident, &ident_location) {
                        Ok((None,None)) => fail!(),
                        Ok((Some(param1),None)) => {
                            params.push(param1);
                        },
                        Ok((Some(param1),Some(location))) => {
                            params.push(param1);
                            option_eq_location = Some(location);
                            break;
                        },
                        Ok((None,Some(location))) => {
                            option_eq_location = Some(location);
                            break;
                        },
                        Err(error) => { return Some(Err(error)); },
                    }
                }
                let eq_location = option_eq_location.unwrap();
                let mut end_location = eq_location.clone();
                let mut body = ~[];
                loop {
                    match tokens.next() {
                        None => {
                            return Some(Err(Error::new_range(ident_location.clone(), end_location.clone(), format!("unexpected EOF in body for `{}`", ident.to_str()))));
                        }
                        Some(token) => {
                            if token.is_dot() {
                                return Some(Ok(Def1 {
                                            ident: ident,
                                            ident_location: ident_location,
                                            params: params,
                                            eq_location: eq_location,
                                            body: body,
                                            dot_location: token.location().clone(),
                                        }));
                            } else {
                                end_location = token.location().clone();
                                body.push(token);
                            }
                        }
                    }
                }
            },
            Some(token) => {
                return Some(Err(Error::new(token.location().clone(), format!("expected identifier, found `{}`", token.to_str()))));
            },
        }
    }

    pub fn head_location(&self) -> Location {
        if self.params.is_empty() {
            self.ident_location.clone()
        } else {
            self.ident_location + self.params.last().location()
        }
    }

    pub fn location(&self) -> Location {
        self.ident_location + self.dot_location
    }
}

impl Param1 {
    fn parse(tokens: &mut Iterator<Token>, name: &Symbol, location: &Location) -> Result<(Option<Param1>,Option<Location>),Error> {
        let mut bits = ~[];
        loop {
            match tokens.next() {
                None => {
                    return Err(Error::new(location.clone(), format!("unexpected EOF in definition for `{}`", name.to_str())));
                },
                Some(Equal(ref location)) => {
                    if bits.is_empty() {
                        return Ok((None,Some(location.clone())));
                    } else {
                        return Ok((Some(Param1 {
                                        bits: bits,
                                        param_type: Param1Dot(None),
                                    }),Some(location.clone())));
                    }
                },
                Some(Zero(ref location)) => {
                    bits.push(ParamBit1 { bit:false, location: location.clone() });
                },
                Some(One(ref location)) => {
                    bits.push(ParamBit1 { bit:true, location: location.clone() });
                },
                Some(Nil(ref location)) => {
                    return Ok((Some(Param1 {
                                    bits: bits,
                                    param_type: Param1Nil(location.clone()),
                                }),None));
                },
                Some(Dot(ref location)) => {
                    return Ok((Some(Param1 {
                                    bits: bits,
                                    param_type: Param1Dot(Some(location.clone())),
                                }),None));
                },
                Some(Ident(ref symbol,ref location)) => {
                    return Ok((Some(Param1 {
                                bits: bits,
                                    param_type: Param1Ident(symbol.clone(),location.clone()),
                                }),None));
                },
            }
        }
    }

    fn is_dot(&self) -> bool {
        match self.param_type {
            Param1Dot(_) => true,
            _ => false,
        }
    }

    fn is_nil(&self) -> bool {
        match self.param_type {
            Param1Nil(_) => true,
            _ => false,
        }
    }

    fn is_ident(&self) -> bool {
        match self.param_type {
            Param1Ident(_,_) => true,
            _ => false,
        }
    }

    fn ident(&self) -> Option<Symbol> {
        match self.param_type {
            Param1Ident(ref symbol,_) => Some(symbol.clone()),
            _ => None,
        }
    }

    fn location_without_bits(&self) -> Option<Location> {
        match self.param_type {
            Param1Nil(ref location) => Some(location.clone()),
            Param1Dot(Some(ref location)) => Some(location.clone()),
            Param1Dot(None) => None,
            Param1Ident(_,ref location) => Some(location.clone()),
        }
    }

    fn location(&self) -> Location {
        match self.location_without_bits() {
            None => {
                assert!(!self.bits.is_empty());
                self.bits.head().location + self.bits.last().location
            },
            Some(ref location) => {
                if self.bits.is_empty() {
                    location.clone()
                } else {
                    self.bits.head().location + *location
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::mem::MemReader;
    use error::Error;
    use parse1::Parse1;
    use symbol::Symbols;
    use token::Token;

    fn parse(src: &str, symbols: Symbols) -> Result<Parse1,~[Error]> {
        let mut tokens = Token::tokenize_buffer("-", ~MemReader::new(src.to_str().into_bytes()), symbols);
        Parse1::parse(&mut tokens)
    }

    fn names(parse1: &Parse1) -> ~[~str] {
        parse1.names().map(|s| s.to_str())
    }

    #[test]
    fn test_error() {
        let symbols = Symbols::new();
        assert!(~"-:1:1: 1:1: expected identifier, found `=`" == parse("=error.", symbols.clone()).unwrap_err()[0].to_str());
        assert!(~"-:1:1: 1:2: unexpected EOF in body for `a`" == parse("a=", symbols.clone()).unwrap_err()[0].to_str());
        assert!(~"-:1:1: 1:1: unexpected EOF in definition for `a`" == parse("a0123", symbols.clone()).unwrap_err()[0].to_str());
    }

    #[test]
    fn test_parse1() {
        let symbols = Symbols::new();
        let parse1 = parse("!1r=0! r.!0r=1! r.!_=_.", symbols.clone()).unwrap();
        assert!(1 == parse1.len());
        assert!([~"!"] == names(&parse1));
        let defs = parse1.find(&symbols.intern_str("!")).unwrap();
        assert!(3 == defs.len());

        assert!("!" == defs[0].ident.to_str());
        assert!(1 == defs[0].params.len());
        assert!(defs[0].params[0].is_ident());
        assert!(1 == defs[0].params[0].bits.len());
        assert!(true == defs[0].params[0].bits[0].bit);
        assert!("r" == defs[0].params[0].ident().unwrap().to_str());
        assert!(3 == defs[0].body.len());
        assert!("-:1:1: 1:3" == defs[0].head_location().to_str());
        assert!("-:1:1: 1:9" == defs[0].location().to_str());

        assert!("!" == defs[1].ident.to_str());
        assert!(1 == defs[1].params.len());
        assert!(defs[1].params[0].is_ident());
        assert!(1 == defs[1].params[0].bits.len());
        assert!(false == defs[1].params[0].bits[0].bit);
        assert!("r" == defs[1].params[0].ident().unwrap().to_str());
        assert!(3 == defs[1].body.len());
        assert!("-:1:10: 1:12" == defs[1].head_location().to_str());
        assert!("-:1:10: 1:18" == defs[1].location().to_str());

        assert!("!" == defs[2].ident.to_str());
        assert!(1 == defs[2].params.len());
        assert!(defs[2].params[0].is_nil());
        assert!(0 == defs[2].params[0].bits.len());
        assert!(1 == defs[2].body.len());
        assert!("-:1:19: 1:20" == defs[2].head_location().to_str());
        assert!("-:1:19: 1:23" == defs[2].location().to_str());

        let parse1 = parse("a=1b.b=0a.", symbols.clone()).unwrap();
        assert!(2 == parse1.len());
        assert!([~"a", ~"b"] == names(&parse1)
                || [~"b", ~"a"] == names(&parse1));
        assert!(parse1.find(&symbols.intern_str("c")).is_none());

        let a = parse1.find(&symbols.intern_str("a")).unwrap();
        assert!(1 == a.len());
        assert!("a" == a[0].ident.to_str());
        assert!(a[0].params.is_empty());
        assert!([~"1", ~"b"] == a[0].body.map(|t| t.to_str()));

        let b = parse1.find(&symbols.intern_str("b")).unwrap();
        assert!(1 == b.len());
        assert!("b" == b[0].ident.to_str());
        assert!(b[0].params.is_empty());
        assert!([~"0", ~"a"] == b[0].body.map(|t| t.to_str()));

        let parse1 = parse("a.=_.", symbols.clone()).unwrap();
        assert!(1 == parse1.len());
        assert!([~"a"] == names(&parse1));
        let a = parse1.find(&symbols.intern_str("a")).unwrap();
        assert!(1 == a.len());
        assert!("a" == a[0].ident.to_str());
        assert!(1 == a[0].params.len());
        assert!(a[0].params[0].is_dot());
    }

    #[test]
    fn test_verify() {
        let symbols = Symbols::new();
        let errors = parse("a0213=.a0=.a1=.a..=.", symbols.clone()).unwrap_err();
        assert!([~"-:1:8: 1:9: `a` defined with 1 argument(s), originally defined with 2 argument(s)",
                 ~"-:1:12: 1:13: `a` defined with 1 argument(s), originally defined with 2 argument(s)",
                 ~"-:1:1: 1:5: `a` originally defined with 2 argument(s)"]
                == errors.map(|e| e.to_str()));
    }
}
