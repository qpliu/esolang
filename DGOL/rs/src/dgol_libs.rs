use std::io;
use std::io::{Read,Write};
use super::interp::{Program,Module,Routine};
use super::nodes::{Node,NodePool};
use super::scope::Scope;

pub fn dgol_libs() -> Box<[Box<Module>]> {
    Box::new([
            Box::new(DGOLLib{
                    name: "IO",
                    routines: Box::new([
                            DGOLLibRoutine{
                                name: "READBYTE",
                                routine: dgol_lib_io_readbyte,
                            },
                            DGOLLibRoutine{
                                name: "WRITEBYTE",
                                routine: dgol_lib_io_writebyte,
                            },
                            ]),
                }),
            ])
}

struct DGOLLib {
    name: &'static str,
    routines: Box<[DGOLLibRoutine]>,
}

impl Module for DGOLLib {
    fn name(&self) -> &str {
        self.name
    }

    fn src_location(&self) -> Option<&(usize,usize)> {
        None
    }

    fn routine_count(&self) -> usize {
        self.routines.len()
    }

    fn routine(&self, index: usize) -> &Routine {
        &self.routines[index] as &Routine
    }

    fn program(&self) -> Option<&Routine> {
        None
    }
}

struct DGOLLibRoutine {
    name: &'static str,
    routine: fn(&mut Scope, &mut NodePool),
}

impl Routine for DGOLLibRoutine {
    fn name(&self) -> &str {
        self.name
    }

    fn src_location(&self) -> Option<&(usize,usize)> {
        None
    }

    fn exported(&self) -> bool {
        true
    }

    fn execute(&self, _: &Program, scope: &mut Scope, node_pool: &mut NodePool) {
        (self.routine)(scope, node_pool);
    }
}

fn dgol_lib_io_readbyte(scope: &mut Scope, node_pool: &mut NodePool) {
    let mut buffer = [0u8];
    if match io::stdin().read(&mut buffer[..]) {
        Ok(n) => n == 0,
        Err(_) => true,
    } {
        add_edge(scope.call_arg(0), scope.call_arg(1), scope, node_pool);
        return;
    }
    remove_edge(scope.call_arg(0), scope.call_arg(1));
    if buffer[0] & 1 == 0 {
        remove_edge(scope.call_arg(0), scope.call_arg(2));
    } else {
        add_edge(scope.call_arg(0), scope.call_arg(2), scope, node_pool);
    }
    if buffer[0] & 2 == 0 {
        remove_edge(scope.call_arg(0), scope.call_arg(3));
    } else {
        add_edge(scope.call_arg(0), scope.call_arg(3), scope, node_pool);
    }
    if buffer[0] & 4 == 0 {
        remove_edge(scope.call_arg(0), scope.call_arg(4));
    } else {
        add_edge(scope.call_arg(0), scope.call_arg(4), scope, node_pool);
    }
    if buffer[0] & 8 == 0 {
        remove_edge(scope.call_arg(0), scope.call_arg(5));
    } else {
        add_edge(scope.call_arg(0), scope.call_arg(5), scope, node_pool);
    }
    if buffer[0] & 16 == 0 {
        remove_edge(scope.call_arg(0), scope.call_arg(6));
    } else {
        add_edge(scope.call_arg(0), scope.call_arg(6), scope, node_pool);
    }
    if buffer[0] & 32 == 0 {
        remove_edge(scope.call_arg(0), scope.call_arg(7));
    } else {
        add_edge(scope.call_arg(0), scope.call_arg(7), scope, node_pool);
    }
    if buffer[0] & 64 == 0 {
        remove_edge(scope.call_arg(0), scope.call_arg(8));
    } else {
        add_edge(scope.call_arg(0), scope.call_arg(8), scope, node_pool);
    }
    if buffer[0] & 128 == 0 {
        remove_edge(scope.call_arg(0), scope.call_arg(9));
    } else {
        add_edge(scope.call_arg(0), scope.call_arg(9), scope, node_pool);
    }
}

fn add_edge(src_node: *mut Option<Node>, dest_node: *mut Option<Node>, scope: &mut Scope, node_pool: &mut NodePool) {
    let mut src_node_ref = unsafe { src_node.as_mut() };
    let mut dest_node_ref = unsafe { dest_node.as_mut() };
    for src_ptr in src_node_ref.iter_mut() {
        for dest_ptr in dest_node_ref.iter_mut() {
            let src = match src_ptr {
                &mut &mut ref mut src@None => {
                    let new_node = scope.new_node(node_pool);
                    *src = Some(new_node.clone());
                    new_node
                },
                &mut &mut Some(ref src) => src.clone(),
            };
            match dest_ptr {
                &mut &mut ref mut dest@None => {
                    let new_node = scope.new_node(node_pool);
                    src.add_edge(&new_node);
                    *dest = Some(new_node);
                    
                },
                &mut &mut Some(ref dest) => {
                    src.add_edge(dest);
                },
            }
        }
    }
}

fn remove_edge(src_node: *mut Option<Node>, dest_node: *mut Option<Node>) {
    let src_node_ref = unsafe { src_node.as_ref() };
    let dest_node_ref = unsafe { dest_node.as_ref() };
    for src_ptr in src_node_ref.iter() {
        for src in src_ptr.iter() {
            for dest_ptr in dest_node_ref.iter() {
                for dest in dest_ptr.iter() {
                    src.remove_edge(&dest);
                }
            }
        }
    }
}

fn dgol_lib_io_writebyte(scope: &mut Scope, _node_pool: &mut NodePool) {
    let mut buffer = [0u8];
    if has_edge(scope.call_arg(0), scope.call_arg(1)) {
        buffer[0] |= 1;
    }
    if has_edge(scope.call_arg(0), scope.call_arg(2)) {
        buffer[0] |= 2;
    }
    if has_edge(scope.call_arg(0), scope.call_arg(3)) {
        buffer[0] |= 4;
    }
    if has_edge(scope.call_arg(0), scope.call_arg(4)) {
        buffer[0] |= 8;
    }
    if has_edge(scope.call_arg(0), scope.call_arg(5)) {
        buffer[0] |= 16;
    }
    if has_edge(scope.call_arg(0), scope.call_arg(6)) {
        buffer[0] |= 32;
    }
    if has_edge(scope.call_arg(0), scope.call_arg(7)) {
        buffer[0] |= 64;
    }
    if has_edge(scope.call_arg(0), scope.call_arg(8)) {
        buffer[0] |= 128;
    }
    let _ = io::stdout().write(&buffer[..]);
}

fn has_edge(src_node: *mut Option<Node>, dest_node: *mut Option<Node>) -> bool {
    let src_node_ref = unsafe { src_node.as_ref() };
    let dest_node_ref = unsafe { dest_node.as_ref() };
    for src_ptr in src_node_ref.iter() {
        for src in src_ptr.iter() {
            for dest_ptr in dest_node_ref.iter() {
                for dest in dest_ptr.iter() {
                    return src.has_edge(&dest);
                }
            }
        }
    }
    false
}
