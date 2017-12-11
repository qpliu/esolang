use std::ptr;
use super::nodes::{Node,NodePool};

pub struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    params: Box<[*mut Option<Node>]>,
    vars: Box<[Option<Node>]>,
    do_edges: Box<[Option<Vec<Node>>]>,
    call_args: Box<[*mut Option<Node>]>,
}

impl<'a> Scope<'a> {
    pub fn new(var_count: usize, do_edges_count: usize, max_call_arg_count: usize) -> Self {
        let mut vars = Vec::with_capacity(var_count);
        vars.resize(var_count, None);
        let mut do_edges = Vec::with_capacity(do_edges_count);
        do_edges.resize(do_edges_count, None);
        let mut call_args = Vec::with_capacity(max_call_arg_count);
        call_args.resize(max_call_arg_count, ptr::null_mut());
        Scope{
            parent: None,
            params: vec![].into_boxed_slice(),
            vars: vars.into_boxed_slice(),
            do_edges: do_edges.into_boxed_slice(),
            call_args: call_args.into_boxed_slice(),
        }
    }

    pub fn push(&'a self, param_count: usize, var_count: usize, do_edges_count: usize, max_call_arg_count: usize) -> Self {
        assert!(var_count >= param_count);
        let mut vars = Vec::with_capacity(var_count);
        vars.resize(var_count, None);
        let mut do_edges = Vec::with_capacity(do_edges_count);
        do_edges.resize(do_edges_count, None);
        let mut params = Vec::with_capacity(param_count);
        for i in 0 .. param_count {
            if i < self.call_args.len() && !self.call_args[i].is_null() {
                params.push(self.call_args[i]);
            } else {
                params.push(&mut vars[i] as *mut Option<Node>);
            }
        }
        let mut call_args = Vec::with_capacity(max_call_arg_count);
        call_args.resize(max_call_arg_count, ptr::null_mut());
        Scope{
            parent: Some(self),
            params: params.into_boxed_slice(),
            vars: vars.into_boxed_slice(),
            do_edges: do_edges.into_boxed_slice(),
            call_args: call_args.into_boxed_slice(),
        }
    }

    pub fn new_node(&self, node_pool: &mut NodePool) -> Node {
        loop {
            match node_pool.new_node() {
                Some(node) => return node,
                None => (),
            }
            node_pool.gc_start();
            self.gc_mark(node_pool);
            node_pool.gc_sweep();
        }
    }

    fn gc_mark(&self, node_pool: &mut NodePool) {
        for option_var in self.vars.iter() {
            for var in option_var.iter() {
                node_pool.gc_mark(var);
            }
        }
        for option_edges in self.do_edges.iter() {
            for edges in option_edges.iter() {
                for edge in edges.iter() {
                    node_pool.gc_mark(edge);
                }
            }
        }
        match self.parent {
            None => (),
            Some(ref parent) => parent.gc_mark(node_pool),
        }
    }

    pub fn clear_call_args(&mut self) {
        for i in 0 .. self.call_args.len() {
            self.call_args[i] = ptr::null_mut();
        }
    }

    pub fn set_call_arg(&mut self, call_arg_index: usize, var_index: usize) {
        assert!(call_arg_index < self.call_args.len());
        assert!(var_index < self.vars.len());
        if var_index < self.params.len() {
            self.call_args[call_arg_index] = self.params[var_index];
        } else {
            self.call_args[call_arg_index] = &mut self.vars[var_index] as *mut Option<Node>;
        }
    }

    pub fn get_var(&mut self, var_index: usize, mut node_pool: &mut NodePool) -> Node {
        assert!(var_index < self.vars.len());
        if var_index < self.params.len() {
            unsafe {
                match *self.params[var_index] {
                    Some(ref node) => node.clone(),
                    None => {
                        let node = self.new_node(&mut node_pool);
                        *self.params[var_index] = Some(node.clone());
                        node
                    },
                }
            }
        } else {
            match self.vars[var_index] {
                Some(ref node) => node.clone(),
                None => {
                    let node = self.new_node(&mut node_pool);
                    self.vars[var_index] = Some(node.clone());
                    node
                }
            }
        }
    }

    pub fn set_var(&mut self, var_index: usize, node: &Node) {
        assert!(var_index < self.vars.len());
        if var_index < self.params.len() {
            unsafe {
                *self.params[var_index] = Some(node.clone());
            }
        } else {
            self.vars[var_index] = Some(node.clone());
        }
    }

    pub fn do_edges_start(&mut self, do_edges_index: usize, node: &Node) {
        assert!(do_edges_index < self.do_edges.len());
        self.do_edges[do_edges_index] = Some(node.edges());
    }

    pub fn do_edges_next(&mut self, do_edges_index: usize) -> Option<Node> {
        assert!(do_edges_index < self.do_edges.len());
        for do_edges in self.do_edges[do_edges_index].iter_mut() {
            return do_edges.pop();
        }
        panic!()
    }

    pub fn do_edges_end(&mut self, do_edges_index: usize) {
        assert!(do_edges_index < self.do_edges.len());
        self.do_edges[do_edges_index] = None;
    }
}
