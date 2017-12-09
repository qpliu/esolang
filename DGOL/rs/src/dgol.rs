mod nodes;
mod scope;

fn main() {
    let mut node_pool = nodes::NodePool::new();
    node_pool.gc_start();
    for node in node_pool.new_node().iter() {
        node_pool.gc_mark(node);
        node.add_edge(node);
        if node.has_edge(node) {
            node.remove_edge(node);
        }
        let _ = node.edges();
    }
    node_pool.gc_sweep();
    let mut scope = scope::Scope::new(1,1,1);
}
