use std;

pub struct Scope<'a> {
    scope_header: ScopeHeader<'a>,
    params: Box<[*mut Option<super::nodes::Node>]>,
    vars: Box<[Option<super::nodes::Node>]>,
    do_edges: Box<[Option<Vec<super::nodes::Node>>]>,
    call_args: Box<[*mut Option<super::nodes::Node>]>,
}

enum ScopeHeader<'a> {
    Root(super::nodes::NodePool),
    Parent(&'a mut Scope<'a>),
}

impl<'a> Scope<'a> {
    pub fn new(var_count: usize, do_edges_count: usize, max_call_arg_count: usize) -> Self {
        let mut vars = Vec::with_capacity(var_count);
        vars.resize(var_count, None);
        let mut do_edges = Vec::with_capacity(do_edges_count);
        do_edges.resize(do_edges_count, None);
        let mut call_args = Vec::with_capacity(max_call_arg_count);
        call_args.resize(max_call_arg_count, std::ptr::null_mut());
        Scope{
            scope_header: ScopeHeader::Root(super::nodes::NodePool::new()),
            params: vec![].into_boxed_slice(),
            vars: vars.into_boxed_slice(),
            do_edges: do_edges.into_boxed_slice(),
            call_args: call_args.into_boxed_slice(),
        }
    }

    pub fn push(&'a mut self, param_count: usize, var_count: usize, do_edges_count: usize, max_call_arg_count: usize, args: &[*mut Option<super::nodes::Node>]) -> Self {
        let mut vars = Vec::with_capacity(var_count);
        vars.resize(var_count, None);
        let mut do_edges = Vec::with_capacity(do_edges_count);
        do_edges.resize(do_edges_count, None);
        let mut params = Vec::with_capacity(param_count);
        for i in 0 .. param_count {
            if i < args.len() {
                params.push(args[i]);
            } else {
                params.push(&mut vars[i] as *mut Option<super::nodes::Node>);
            }
        }
        let mut call_args = Vec::with_capacity(max_call_arg_count);
        call_args.resize(max_call_arg_count, std::ptr::null_mut());
        Scope{
            scope_header: ScopeHeader::Parent(self),
            params: params.into_boxed_slice(),
            vars: vars.into_boxed_slice(),
            do_edges: do_edges.into_boxed_slice(),
            call_args: call_args.into_boxed_slice(),
        }
    }

    fn node_pool(&mut self) -> &mut super::nodes::NodePool {
        match self.scope_header {
            ScopeHeader::Root(ref mut node_pool) => node_pool,
            ScopeHeader::Parent(ref mut parent) => parent.node_pool(),
        }
    }
}
