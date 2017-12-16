use std::collections::HashMap;
use std::io::{Error,ErrorKind,Result,Read};

use super::interp;
use super::scope::Scope;

pub struct Module {
    name: Box<str>,
    uses: Box<[Box<str>]>,
    routines: Box<[Routine]>,
    exports: Box<[Box<str>]>,
    program: Option<Routine>,
}

pub struct Routine {
    name: Box<str>,
    parameters: Box<[Box<str>]>,
    statements: Box<[Statement]>,
    exported: bool,

    var_count: usize,
    do_edges_count: usize,
}

pub enum Statement {
    LetEq {
        args: [Box<str>; 2],
        arg_indexes: [isize; 2],
    },
    LetAddEdge {
        args: [Box<str>; 2],
        arg_indexes: [isize; 2],
    },
    LetRemoveEdge {
        args: [Box<str>; 2],
        arg_indexes: [isize; 2],
    },
    If(Box<[IfBranch]>),
    Call {
        module_name: Option<Box<str>>,
        routine_name: Box<str>,
        args: Box<[Box<str>]>,
        module_index: usize,
        routine_index: usize,
        arg_indexes: Box<[usize]>,
    },
    Return,
    DoLoop {
        arg: Box<str>,
        arg_index: usize,
    },
    DoEdges {
        args: [Box<str>; 2],
        arg_indexes: [usize; 2],
    },
    Exit {
        arg: Box<str>,
        arg_index: usize,
    },
}

pub enum IfBranch {
    Eq {
        args: [Box<str>; 2],
        arg_indexes: [usize; 2],
        statements: Box<[Statement]>,
    },
    Edges {
        args: [Box<str>; 2],
        arg_indexes: [usize; 2],
        statements: Box<[Statement]>,
    },
    Else {
        statements: Box<[Statement]>,
    },
}

impl interp::Module for Module {
    fn name(&self) -> &str {
        &self.name
    }

    fn routine_count(&self) -> usize {
        self.routines.len()
    }

    fn routine(&self, index: usize) -> &interp::Routine {
        &self.routines[index] as &interp::Routine
    }

    fn program(&self) -> Option<&interp::Routine> {
        self.program.as_ref().map(|r| r as &interp::Routine)
    }
}

impl interp::Routine for Routine {
    fn name(&self) -> &str {
        &self.name
    }

    fn exported(&self) -> bool {
        self.exported
    }

    fn execute(&self, program: &interp::Program, scope: &mut Scope) {
        panic!("not implemented")
    }
}

pub fn err<T>(msg: &str) -> Result<T> {
    Err(Error::new(ErrorKind::Other, msg))
}

impl Module {
    pub fn parse(filename: &str, r: &mut Read) -> Result<Self> {
        panic!("not implemented")
    }

    pub fn resolve(&mut self, module_index: usize, module_routine_indexes: &HashMap<String, (usize,HashMap<String,(usize,bool)>)>) -> Result<()> {
        for routine in self.routines.iter_mut() {
            routine.resolve(module_index, module_routine_indexes)?;
        }
        for routine in self.program.iter_mut() {
            routine.resolve(module_index, module_routine_indexes)?;
        }
        Ok(())
    }
}

impl Routine {
    fn resolve(&mut self, module_index: usize, module_routine_indexes: &HashMap<String, (usize,HashMap<String,(usize,bool)>)>) -> Result<()> {
        panic!("not implemented")
    }
}
