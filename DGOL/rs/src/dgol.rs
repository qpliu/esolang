use std::env;
use std::fs::File;
use std::io::{Result,Write,stderr};
use std::process;

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
            process::exit(1);
        },
    }
}

fn program() -> Result<interp::Program> {
    let mut modules = Vec::new();
    for (i,arg) in env::args().skip(1).enumerate() {
        let f = File::open(&arg)?;
        let module = ast::Module::parse(i, f)?;
        modules.push(module);
    }
    interp::Program::resolve(modules, dgol_libs::dgol_libs())
}
