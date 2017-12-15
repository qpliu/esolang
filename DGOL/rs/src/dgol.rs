mod ast;
mod dgol_libs;
mod interp;
mod nodes;
mod scope;

fn main() {
    let mut node_pool = nodes::NodePool::new();
    let scope = scope::Scope::new(1,1,1);
    let mut new_scope = scope.push(1,1,1,1);
    let node = new_scope.new_node(&mut node_pool);
    node.add_edge(&node);
    if node.has_edge(&node) {
        node.remove_edge(&node);
    }
    node.edges();
    new_scope.clear_call_args();
    new_scope.set_call_arg(0,1);
    new_scope.set_var(0, &node);
    new_scope.get_var(0, &mut node_pool);
    new_scope.do_edges_start(0, &node);
    new_scope.do_edges_next(0);
    new_scope.do_edges_end(0);
    dgol_libs::dgol_libs();
}
