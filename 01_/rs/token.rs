use std::io::File;
use std::io::buffered::BufferedReader;
use std::io::Buffer;
use std::rc::Rc;

use location::Location;
use symbol::{Symbol,Symbols};

pub enum Token {
    Zero(Location),
    One(Location),
    Nil(Location),
    Equal(Location),
    Dot(Location),
    Ident(Symbol,Location),
}

pub struct TokenIterator {
    priv file_name: Rc<~str>,
    priv buffer: ~Buffer,
    priv line_number: uint,
    priv column: uint,
    priv line: ~[char],
    priv eof: bool,
    priv symbols: Symbols,
}

pub struct MultiTokenIterator {
    priv iters: ~[TokenIterator],
}

impl Token {
    pub fn tokenize(file_names: &[Path], symbols: Symbols) -> MultiTokenIterator {
        MultiTokenIterator { iters: file_names.map(|file_name| Token::tokenize_buffer(file_name.as_str().unwrap_or("-"), ~BufferedReader::new(File::open(file_name)), symbols.clone())) }
    }

    pub fn tokenize_buffer(file_name: &str, buffer: ~Buffer, symbols: Symbols) -> TokenIterator {
        TokenIterator {
            file_name: Rc::new(file_name.to_owned()),
            buffer: buffer,
            line_number: 0,
            column: 0,
            line: ~[],
            eof: false,
            symbols: symbols,
        }
    }

    pub fn is_zero(&self) -> bool {
        match *self { Zero(_) => true, _ => false }
    }

    pub fn is_one(&self) -> bool {
        match *self { One(_) => true, _ => false }
    }

    pub fn is_nil(&self) -> bool {
        match *self { Nil(_) => true, _ => false }
    }

    #[cfg(test)]
    pub fn is_equal(&self) -> bool {
        match *self { Equal(_) => true, _ => false }
    }

    pub fn is_dot(&self) -> bool {
        match *self { Dot(_) => true, _ => false }
    }

    pub fn is_ident(&self) -> bool {
        match *self { Ident(_,_) => true, _ => false }
    }

    pub fn ident(&self) -> Option<Symbol> {
        match *self { Ident(ref symbol,_) => Some(symbol.clone()), _ => None }
    }

    pub fn location<'a>(&'a self) -> &'a Location {
        match *self {
            Zero(ref location) => location,
            One(ref location) => location,
            Nil(ref location) => location,
            Equal(ref location) => location,
            Dot(ref location) => location,
            Ident(_,ref location) => location,
        }
    }
}

impl ToStr for Token {
    fn to_str(&self) -> ~str {
        match *self {
            Zero(_) => ~"0",
            One(_) => ~"1",
            Nil(_) => ~"_",
            Equal(_) => ~"=",
            Dot(_) => ~".",
            Ident(ref name,_) => name.to_str(),
        }
    }
}

impl Iterator<Token> for TokenIterator {
    fn next(&mut self) -> Option<Token> {
        if self.eof {
            return None;
        }
        loop {
            if self.column >= self.line.len() {
                let line = self.buffer.read_line().unwrap_or_else(|| {
                    self.eof = true;
                    ~""
                });
                if self.eof {
                    return None;
                }
                self.line = line.chars().collect();
                self.column = 0;
                self.line_number += 1;
            }                
            match self.line[self.column] {
                '0' => { return Some(Zero(self.advance(self.column+1))) },
                '1' => { return Some(One(self.advance(self.column+1))) },
                '_' => { return Some(Nil(self.advance(self.column+1))) },
                '.' => { return Some(Dot(self.advance(self.column+1))) },
                ' ' | '\t' | '\r' | '\n' => {
                    self.column += 1;
                },
                '=' => {
                    if self.column+1 < self.line.len() && self.line[self.column+1] == '=' {
                        self.column = self.line.len();
                    } else {
                        return Some(Equal(self.advance(self.column+1)));
                    }
                },
                _ => { return Some(self.ident()); },
            }
        }
    }
}

impl TokenIterator {
    fn advance(&mut self, column: uint) -> Location {
        let start_column = self.column;
        self.column = column;
        Location {
            file_name: self.file_name.clone(),
            start_line: self.line_number,
            start_column: start_column,
            end_line: self.line_number,
            end_column: column,
        }
    }

    fn ident(&mut self) -> Token {
        for column in range(self.column, self.line.len()) {
            match self.line[column] {
                '0' | '1' | '_' | '.' | '=' | ' ' | '\t' | '\r' | '\n' => {
                    return self.advance_ident(column);
                },
                _ => (),
            }
        }
        let end_column = self.line.len();
        self.advance_ident(end_column)
    }

    fn advance_ident(&mut self, end_column: uint) -> Token {
        let ident = self.symbols.intern(self.line.slice(self.column, end_column));
        Ident(ident, self.advance(end_column))
    }
}

impl Iterator<Token> for MultiTokenIterator {
    fn next(&mut self) -> Option<Token> {
        while !self.iters.is_empty() {
            match self.iters[0].next() {
                None => { self.iters.shift(); }
                item => { return item; }
            }
        }
        return None;
    }
}

#[cfg(test)]
mod tests {
    use super::Token;
    use symbol::Symbols;

    #[test]
    fn test_tokenize() {
        use std::io::mem::MemReader;
        let expected = ["cat", "a", "_", "=", "0", "1", "a", "."].map(|s| s.to_owned());
        let tokens : ~[Token] = Token::tokenize_buffer("-", ~MemReader::new(bytes!("cat a\n _ = == comment \n 01a.").to_owned()), Symbols::new()).collect();
        assert!(expected == tokens.map(|t| t.to_str()));

        let expected = ["-:1:1: 1:3", "-:1:5: 1:5", "-:2:2: 2:2", "-:2:4: 2:4", "-:3:2: 3:2", "-:3:3: 3:3", "-:3:4: 3:4", "-:3:5: 3:5"].map(|s| s.to_owned());
        assert!(expected == tokens.map(|t| { t.location().to_str() }));
        assert!(~[false, false, false, false, true, false, false, false] == tokens.map(|t| t.is_zero()));
        assert!(~[false, false, false, false, false, true, false, false] == tokens.map(|t| t.is_one()));
        assert!(~[false, false, true, false, false, false, false, false] == tokens.map(|t| t.is_nil()));
        assert!(~[false, false, false, true, false, false, false, false] == tokens.map(|t| t.is_equal()));
        assert!(~[false, false, false, false, false, false, false, true] == tokens.map(|t| t.is_dot()));
        assert!(~[true, true, false, false, false, false, true, false] == tokens.map(|t| t.is_ident()));
    }

    #[test]
    fn test_tokenize_file() {
        use std::io::fs;
        use std::libc;
        use std::os;
        use std::io::File;
        let path = os::tmpdir().join(format!("testtoken{}", unsafe { libc::getpid() }));
        File::create(&path).write(bytes!("id x=x."));
        let tokens : ~[Token] = Token::tokenize([path.clone()], Symbols::new()).collect();
        fs::unlink(&path);
        assert!(["id", "x", "=", "x", "."].map(|s| s.to_owned()) == tokens.map(|t| t.to_str()));
    }
}
