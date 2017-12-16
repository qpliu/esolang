use std::collections::HashMap;
use std::io::Result;

use super::ast;
use super::scope::Scope;

pub struct Program {
    modules: Box<[Box<Module>]>,
    program_module_index: usize,
}

pub trait Module {
    fn name(&self) -> &str;
    fn routine_count(&self) -> usize;
    fn routine(&self, index: usize) -> &Routine;
    fn program(&self) -> Option<&Routine>;
}

pub trait Routine {
    fn name(&self) -> &str;
    fn exported(&self) -> bool;
    fn execute(&self, program: &Program, scope: &mut Scope);
}

impl Program {
    pub fn resolve(modules: Vec<ast::Module>, libs: Box<[Box<Module>]>) -> Result<Self> {
        let mut program_module_index = None;
        let mut module_routine_indexes = HashMap::new();
        let mut module_index = 0;
        for module in modules.iter() {
            if module_routine_indexes.contains_key(module.name()) {
                return ast::err(&format!("DUPLICATE MODULE NAME: {}", module.name()));
            }
            let mut routine_indexes = HashMap::new();
            for index in 0 .. module.routine_count() {
                let routine = module.routine(index);
                if routine_indexes.contains_key(routine.name()) {
                    return ast::err(&format!("DUPLICATE SUBROUTINE NAME: {} IN MODULE: {}", routine.name(), module.name()));
                }
                routine_indexes.insert(routine.name().to_owned(), (index,routine.exported()));
            }
            if module.program().is_some() {
                if program_module_index.is_some() {
                    return ast::err("MULTIPLE PROGRAM MODULES");
                }
                program_module_index = Some(module_index);
            }
            module_routine_indexes.insert(module.name().to_owned(), (module_index,routine_indexes));
            module_index += 1;
        }
        if program_module_index.is_none() {
            return ast::err("NO PROGRAM MODULE");
        }
        let program_module_index = program_module_index.unwrap();
        let mut used_libs = HashMap::new();
        for module in libs.as_ref().iter() {
            if module_routine_indexes.contains_key(module.name()) {
                continue;
            }
            let mut routine_indexes = HashMap::new();
            for index in 0 .. module.routine_count() {
                let routine = module.routine(index);
                routine_indexes.insert(routine.name().to_owned(), (index,routine.exported()));
            }
            module_routine_indexes.insert(module.name().to_owned(), (module_index,routine_indexes));
            used_libs.insert(module.name().to_owned(), ());
            module_index += 1;
        }
        let mut module_vec: Vec<Box<Module>> = Vec::new();
        module_index = 0;
        for mut module in modules.into_iter() {
            module.resolve(module_index, &module_routine_indexes);
            module_vec.push(Box::new(module));
            module_index += 1;
        }
        for module in libs.into_vec().into_iter() {
            if used_libs.contains_key(module.name()) {
                module_vec.push(module);
            }
        }
        
        Ok(Program{
            modules: module_vec.into_boxed_slice(),
            program_module_index: program_module_index,
        })
    }

    pub fn execute(&self) {
        self.modules[self.program_module_index].program().unwrap().execute(&self, &mut Scope::new());
    }
}
