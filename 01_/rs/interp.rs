use ast::{Ast,DefIndex};

pub trait Interp<B> {
    fn new() -> Self;
    fn nil(&mut self) -> B;
    fn constant(&mut self, bits: ~[bool]) -> B;
    fn file(&mut self, file: &Path) -> B;
    fn reader(&mut self, reader: ~Reader) -> B;
    fn run(self, ast: Ast<B>, def_index: DefIndex, args: ~[B], writer: &mut Writer);
}
