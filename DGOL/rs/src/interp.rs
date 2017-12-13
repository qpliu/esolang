use super::scope::Scope;

pub trait Module {
    fn name(&self) -> &str;
    fn routine_index(&self, name: &str) -> Option<usize>;
    fn routine(&self, index: usize) -> &Routine;
}

pub trait Routine {
    fn name(&self) -> &str;
    fn execute(&self, scope: &mut Scope);
}
