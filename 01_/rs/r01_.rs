mod ast;
mod bits;
mod error;
mod interp;
mod location;
mod parse1;
mod symbol;
mod token;

#[cfg(not(test))]
fn main() {
    use std::os;
    let args = os::args();
    fail!();
}
