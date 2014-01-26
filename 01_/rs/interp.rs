use ast::{Ast,DefIndex};
use bits::Bits;

pub trait Interp<B:Bits> {
    fn new(ast: Ast<B>, def_index: DefIndex, args: ~[B]) -> Self;
    fn run(self) -> B;
}
