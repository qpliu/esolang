use super::scope::Scope;

pub struct Program {
    modules: Box<[Box<Module>]>,
    program_module_index: usize,
}

pub trait Module {
    fn name(&self) -> &str;
    fn routine_index(&self, name: &str) -> Option<usize>;
    fn exported_routine_index(&self, name: &str) -> Option<usize>;
    fn routine(&self, index: usize) -> &Routine;
    fn program(&self) -> Option<&Routine>;
}

pub trait Routine {
    fn name(&self) -> &str;
    fn execute(&self, program: &Program, scope: &mut Scope);
}
