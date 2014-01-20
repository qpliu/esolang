use ast::{Ast,DefIndex};
use bits::Bits;

pub fn interp(ast: &Ast, f: DefIndex, args: ~[Bits]) -> Bits {
    fail!()
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_interp() {
    }
}
