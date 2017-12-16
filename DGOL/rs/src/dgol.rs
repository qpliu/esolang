use std::env::args;
use std::fs::File;
use std::io::{Result,Write,stderr};

mod ast;
mod dgol_libs;
mod interp;
mod nodes;
mod scope;

fn main() {
    match program() {
        Ok(program) => {
            program.execute();
        },
        Err(err) => {
            let _ = stderr().write_fmt(format_args!("{}\n", err));
        },
    }
}

fn program() -> Result<interp::Program> {
    let mut modules = Vec::new();
    for arg in args() {
        let mut f = File::open(&arg)?;
        let module = ast::Module::parse(&arg, &mut f)?;
        modules.push(module);
    }
    interp::Program::resolve(modules, dgol_libs::dgol_libs())
}
