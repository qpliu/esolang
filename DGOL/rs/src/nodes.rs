pub struct NodePool {
    gc_mark: u8,
    node_page: NodePage,
}

struct NodePage {
    next_page: Option<Box<NodePage>>,
    nodes: Box<[NodeCell]>,
}

struct NodeCell {
    gc_mark: u8,
    edges: Option<Vec<Node>>,
}

#[derive(PartialEq,Eq,Clone)]
pub struct Node(*mut NodeCell);

impl NodePool {
    pub fn new() -> Self {
        NodePool{
            gc_mark: 0,
            node_page: NodePage::new(),
        }
    }

    pub fn gc_start(&mut self) {
        self.gc_mark += 1;
    }

    pub fn gc_mark(&self, node: &Node) {
        node.gc_mark(self.gc_mark);
    }

    pub fn gc_sweep(&mut self) {
        self.node_page.gc_sweep(self.gc_mark);
    }

    // If new_node returns None, gc and try again.
    pub fn new_node(&mut self) -> Option<Node> {
        self.node_page.new_node(self.gc_mark)
    }
}

const PAGE_SIZE: usize = 256;
const NEW_PAGE_THRESHOLD: usize = 240;

impl NodePage {
    fn new() -> Self {
        let mut nodes = Vec::with_capacity(PAGE_SIZE);
        for _ in 0 .. PAGE_SIZE {
            nodes.push(NodeCell::empty());
        }
        NodePage{
            next_page: None,
            nodes: nodes.into_boxed_slice(),
        }
    }

    fn gc_sweep(&mut self, gc_mark: u8) {
        let mut live_nodes = 0;
        for mut node in self.nodes.iter_mut() {
            if node.is_empty() {
            } else if node.has_gc_mark(gc_mark) {
                live_nodes += 1;
            } else {
                node.free();
            }
        }
        // If this is the last page and there are more than NEW_PAGE_THRESHOLD
        // live nodes after the sweep, allocate a new page.
        if self.next_page.is_some() {
            for mut next_page in self.next_page.iter_mut() {
                next_page.gc_sweep(gc_mark);
            }
        } else {
            if live_nodes > NEW_PAGE_THRESHOLD {
                self.next_page = Some(Box::new(NodePage::new()));
            }
        }
    }

    fn new_node(&mut self, gc_mark: u8) -> Option<Node> {
        for mut node in self.nodes.iter_mut() {
            if node.is_empty() {
                node.alloc(gc_mark);
                return Some(Node(node as *mut NodeCell));
            }
        }
        for mut next_page in self.next_page.iter_mut() {
            return next_page.new_node(gc_mark);
        }
        None
    }
}

impl NodeCell {
    fn empty() -> Self {
        NodeCell{
            gc_mark: 0,
            edges: None,
        }
    }

    fn is_empty(&self) -> bool {
        self.edges.is_none()
    }

    fn alloc(&mut self, gc_mark: u8) {
        assert!(self.is_empty());
        self.gc_mark = gc_mark;
        self.edges = Some(Vec::new());
    }

    fn free(&mut self) {
        assert!(!self.is_empty());
        self.edges.take();
    }

    fn has_edge(&self, other: &Node) -> bool {
        assert!(self.is_empty());
        for edges in self.edges.iter() {
            return edges.contains(other);
        }
        panic!();
    }

    fn add_edge(&mut self, other: &Node) {
        assert!(self.is_empty());
        if !self.has_edge(other) {
            for mut edges in self.edges.iter_mut() {
                edges.push(other.clone());
            }
        }
    }

    fn remove_edge(&mut self, other: &Node) {
        assert!(!self.is_empty());
        for edges in self.edges.iter_mut() {
            let mut index = None;
            for (i, edge) in edges.iter().enumerate() {
                if edge == other {
                    index = Some(i);
                    break;
                }
            }
            index.map(|i| edges.swap_remove(i));
        }
    }

    fn edges(&self) -> Vec<Node> {
        for edges in self.edges.iter() {
            return edges.to_vec();
        }
        panic!();
    }

    fn has_gc_mark(&self, gc_mark: u8) -> bool {
        assert!(self.is_empty());
        self.gc_mark == gc_mark
    }
}

impl Node {
    pub fn has_edge(&self, other: &Node) -> bool {
        unsafe {
            (*self.0).has_edge(other)
        }
    }

    pub fn add_edge(&self, other: &Node) {
        unsafe {
            (*self.0).add_edge(other)
        }
    }

    pub fn remove_edge(&self, other: &Node) {
        unsafe {
            (*self.0).remove_edge(other)
        }
    }

    pub fn edges(&self) -> Vec<Node> {
        unsafe {
            (*self.0).edges()
        }
    }

    fn gc_mark(&self, gc_mark: u8) {
        unsafe {
            if gc_mark == (*self.0).gc_mark {
                return;
            }
            (*self.0).gc_mark = gc_mark;
            for edges in (*self.0).edges.iter() {
                for edge in edges {
                    edge.gc_mark(gc_mark);
                }
            }
        }
    }
}
