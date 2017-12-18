use std::collections::{HashMap,HashSet};
use std::io::{Error,ErrorKind,Result,Read};
use std::marker::PhantomData;

use super::interp;
use super::nodes::NodePool;
use super::scope::Scope;

pub struct Module {
    src_location: (usize,usize),
    name: Box<str>,
    uses: Box<[Box<str>]>,
    routines: Box<[Routine]>,
    exports: Box<[Box<str>]>,
    program: Option<Routine>,
}

pub struct Routine {
    src_location: (usize,usize),
    name: Box<str>,
    parameters: Box<[Box<str>]>,
    statements: Box<[Statement]>,
    exported: bool,

    var_count: usize,
    do_edges_count: usize,
    max_call_arg_count: usize,
}

enum Statement {
    LetEq {
        src_location: (usize,usize),
        args: [Box<str>; 2],
        arg0_index: usize,
        arg1_index: Option<usize>,
    },
    LetAddEdge {
        src_location: (usize,usize),
        args: [Box<str>; 2],
        arg0_index: usize,
        arg1_index: Option<usize>,
    },
    LetRemoveEdge {
        src_location: (usize,usize),
        args: [Box<str>; 2],
        arg_indexes: [usize; 2],
    },
    If(Box<[IfBranch]>),
    Call {
        src_location: (usize,usize),
        module_name: Option<Box<str>>,
        routine_name: Box<str>,
        args: Box<[Box<str>]>,
        module_index: usize,
        routine_index: usize,
        arg_indexes: Box<[Option<usize>]>,
    },
    Return {
        src_location: (usize,usize),
    },
    DoLoop {
        src_location: (usize,usize),
        arg: Box<str>,
        statements: Box<[Statement]>,
        do_index: usize,
    },
    DoEdges {
        src_location: (usize,usize),
        args: [Box<str>; 2],
        statements: Box<[Statement]>,
        arg_indexes: [usize; 2],
        do_index: usize,
        do_edges_index: usize,
    },
    Exit {
        src_location: (usize,usize),
        arg: Box<str>,
        do_index: usize,
    },
}

enum IfBranch {
    Eq {
        src_location: (usize,usize),
        args: [Box<str>; 2],
        arg_indexes: [usize; 2],
        statements: Box<[Statement]>,
    },
    Edges {
        src_location: (usize,usize),
        args: [Box<str>; 2],
        arg_indexes: [usize; 2],
        statements: Box<[Statement]>,
    },
    Else {
        src_location: (usize,usize),
        statements: Box<[Statement]>,
    },
}

impl interp::Module for Module {
    fn name(&self) -> &str {
        &self.name
    }

    fn src_location(&self) -> Option<&(usize,usize)> {
        Some(&self.src_location)
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

    fn src_location(&self) -> Option<&(usize,usize)> {
        Some(&self.src_location)
    }

    fn exported(&self) -> bool {
        self.exported
    }

    fn execute(&self, program: &interp::Program, scope: &mut Scope, node_pool: &mut NodePool) {
        let mut new_scope = Scope::push(scope, self.parameters.len(), self.var_count, self.do_edges_count, self.max_call_arg_count);
        execute_statements(&self.statements, program, &mut new_scope, node_pool);
    }
}

enum Next {
    Exit(usize),
    Return,
    Fallthrough,
}

fn execute_statements(statements: &[Statement], program: &interp::Program, scope: &mut Scope, node_pool: &mut NodePool) -> Next {
    for statement in statements.iter() {
        let next = statement.execute(program, scope, node_pool);
        if let Next::Fallthrough = next {
        } else {
            return next;
        }
    }
    Next::Fallthrough
}

pub fn err<T>(src_location: Option<&(usize,usize)>, msg: &str) -> Result<T> {
    match src_location {
        Some(&(ref filename_index,ref line_number)) => {
            use std::env::args;
            Err(Error::new(ErrorKind::Other, format!("{}:{} {}", args().nth(*filename_index+1).unwrap(), *line_number, msg)))
        },
        None => Err(Error::new(ErrorKind::Other, msg)),
    }
}

impl Module {
    pub fn parse<R: Read>(filename_index: usize, r: R) -> Result<Self> {
        ModuleParser::parse(filename_index, r)
    }

    pub fn resolve(&mut self, module_index: usize, module_routine_indexes: &HashMap<String, (usize,HashMap<String,(usize,bool)>)>) -> Result<()> {
        ModuleResolver::resolve(self, module_index, module_routine_indexes)
    }
}

impl Statement {
    fn execute(&self, program: &interp::Program, scope: &mut Scope, node_pool: &mut NodePool) -> Next {
        match self {
            &Statement::LetEq {
                src_location: _,
                args: _,
                ref arg0_index,
                ref arg1_index,
            } => {
                let node = match arg1_index {
                    &Some(index) => scope.get_var(index, node_pool),
                    &None => scope.new_node(node_pool),
                };
                scope.set_var(*arg0_index, &node);
                Next::Fallthrough
            },
            &Statement::LetAddEdge {
                src_location: _,
                args: _,
                ref arg0_index,
                ref arg1_index,
            } => {
                let node = match arg1_index {
                    &Some(index) => scope.get_var(index, node_pool),
                    &None => scope.new_node(node_pool),
                };
                scope.get_var(*arg0_index, node_pool).add_edge(&node);
                Next::Fallthrough
            },
            &Statement::LetRemoveEdge {
                src_location: _,
                args: _,
                ref arg_indexes,
            } => {
                let node0 = scope.get_var(arg_indexes[0], node_pool);
                let node1 = scope.get_var(arg_indexes[1], node_pool);
                node0.remove_edge(&node1);
                Next::Fallthrough
            },
            &Statement::If(ref if_branches) => {
                for if_branch in if_branches.iter() {
                    match if_branch.execute(program, scope, node_pool) {
                        Some(next) => return next,
                        None => (),
                    }
                }
                Next::Fallthrough
            },
            &Statement::Call {
                src_location: _,
                module_name: _,
                routine_name: _,
                args: _,
                ref module_index,
                ref routine_index,
                ref arg_indexes,
            } => {
                scope.clear_call_args();
                for i in 0 .. arg_indexes.len() {
                    if let Some(arg_index) = arg_indexes[i] {
                        scope.set_call_arg(i, arg_index);
                    }
                }
                program.module(*module_index).routine(*routine_index).execute(program, scope, node_pool);
                Next::Fallthrough
            },
            &Statement::Return {
                src_location: _,
            } => {
                Next::Return
            },
            &Statement::DoLoop {
                src_location: _,
                arg: _,
                ref statements,
                ref do_index,
            } => {
                loop {
                    let next = execute_statements(statements, program, scope, node_pool);
                    match next {
                        Next::Exit(index) => {
                            if index == *do_index {
                                return Next::Fallthrough;
                            }
                            return next;
                        },
                        Next::Fallthrough => (),
                        _ => return next,
                    }
                }
            },
            &Statement::DoEdges {
                src_location: _,
                args: _,
                ref statements,
                ref arg_indexes,
                ref do_index,
                ref do_edges_index,
            } => {
                let node1 = scope.get_var(arg_indexes[1], node_pool);
                scope.do_edges_start(*do_edges_index, &node1);
                let mut next = Next::Fallthrough;
                loop {
                    match scope.do_edges_next(*do_edges_index) {
                        Some(edge) => scope.set_var(arg_indexes[0], &edge),
                        None => break,
                    }
                    let do_next = execute_statements(statements, program, scope, node_pool);
                    match do_next {
                        Next::Exit(index) => {
                            if index != *do_index {
                                next = do_next;
                            }
                            break;
                        },
                        Next::Fallthrough => (),
                        _ => {
                            next = do_next;
                            break;
                        },
                    }
                }
                scope.do_edges_end(*do_edges_index);
                next
            },
            &Statement::Exit {
                src_location: _,
                arg: _,
                ref do_index,
            } => {
                Next::Exit(*do_index)
            },
        }
    }
}

impl IfBranch {
    fn execute(&self, program: &interp::Program, scope: &mut Scope, node_pool: &mut NodePool) -> Option<Next> {
        match self {
            &IfBranch::Eq {
                src_location: _,
                args: _,
                ref arg_indexes,
                ref statements,
            } => {
                let arg0 = scope.get_var(arg_indexes[0], node_pool);
                let arg1 = scope.get_var(arg_indexes[1], node_pool);
                if arg0 == arg1 {
                    return Some(execute_statements(statements, program, scope, node_pool))
                }
            },
            &IfBranch::Edges {
                src_location: _,
                args: _,
                ref arg_indexes,
                ref statements,
            } => {
                let arg0 = scope.get_var(arg_indexes[0], node_pool);
                let arg1 = scope.get_var(arg_indexes[1], node_pool);
                if arg0.has_edge(&arg1) {
                    return Some(execute_statements(statements, program, scope, node_pool))
                }
            },
            &IfBranch::Else {
                src_location: _,
                ref statements,
            } => {
                return Some(execute_statements(statements, program, scope, node_pool));
            },
        }
        None
    }
}

fn is_identifier(token: &str) -> bool {
    token != "0" && is_identifier_or_0(token)
}

fn is_identifier_or_0(token: &str) -> bool {
    for ch in token.chars() {
        if !ch.is_alphanumeric() {
            return false;
        }
    }
    token.len() > 0
}

struct ModuleParser<'a,R:'a> {
    filename_index: usize,
    reader: R,
    current_line: String,
    current_line_number: usize,
    current_token_index: usize,
    seen_eof: bool,
    phantom_data: PhantomData<&'a R>,

    name: String,
    uses: Vec<Box<str>>,
    routines: Vec<Routine>,
    exports: Vec<Box<str>>,
    program: Option<Routine>,
}

struct RoutineParser<'a,R:'a> {
    module_parser: &'a mut ModuleParser<'a,R>,

    src_location: (usize,usize),
    name: String,
    parameters: Vec<Box<str>>,
    statements: Vec<Statement>,
}

impl<'a,R:Read> ModuleParser<'a,R> {
    pub fn parse(filename_index: usize, r: R) -> Result<Module> {
        let mut module_parser = ModuleParser {
            filename_index: filename_index,
            reader: r,
            current_line: String::new(),
            current_line_number: 0,
            current_token_index: 0,
            seen_eof: false,
            phantom_data: PhantomData,

            name: String::new(),
            uses: Vec::new(),
            routines: Vec::new(),
            exports: Vec::new(),
            program: None,
        };
        module_parser.next_line(false)?;
        module_parser.parse_uses()?;
        module_parser.parse_subroutines()?;
        if !module_parser.parse_exports()? {
            if !module_parser.parse_program()? {
                return module_parser.err("SYNTAX ERROR");
            }
        }
        Ok(Module {
                src_location:(filename_index,1),
                name: module_parser.name.into_boxed_str(),
                uses: module_parser.uses.into_boxed_slice(),
                routines: module_parser.routines.into_boxed_slice(),
                exports: module_parser.exports.into_boxed_slice(),
                program: module_parser.program,
            })
    }

    fn next_line(&mut self, expect_eof: bool) -> Result<()> {
        if self.seen_eof {
            if expect_eof {
                return Ok(());
            } else {
                return self.err("UNEXPECTED EOF");
            }
        }
        self.current_line_number += 1;
        self.current_line.clear();
        let mut in_comment = false;
        loop {
            let mut buffer = [0u8; 1];
            let count = self.reader.read(&mut buffer)?;
            if count == 0 {
                self.seen_eof = true;
                if !self.current_line.is_empty() || expect_eof {
                    return Ok(());
                } else {
                    return self.err("UNEXPECTED EOF");
                }
            }
            let ch = char::from(buffer[0]); // Change to decoding UTF-8 when it is better supported in the std lib
            if in_comment {
                if ch == '\n' {
                    if !self.current_line.is_empty() {
                        if !expect_eof {
                            return Ok(());
                        } else {
                            return self.err("UNEXPECTED TRAILING JUNK");
                        }
                    } else {
                        in_comment = false;
                        self.current_line_number += 1;
                    }
                }
            } else {
                if ch == '\n' {
                    if !self.current_line.is_empty() {
                        return Ok(());
                    } else {
                        self.current_line_number += 1;
                    }
                } else if ch == '*' {
                    in_comment = true;
                } else {
                    self.current_line.push(ch);
                }
            }
        }
    }

    fn starts_with(&mut self, token: &str) -> bool {
        if self.current_line.starts_with(token) {
            self.current_token_index = token.len();
            true
        } else {
            false
        }
    }

    fn next_token(&mut self) -> Option<String> {
        let start = self.current_token_index;
        let len = self.current_line.len();
        if start >= len {
            return None;
        }
        let mut token = String::new();
        for ch in self.current_line[start ..].chars() {
            if ch.is_alphanumeric() {
                token.push(ch);
            } else if token.is_empty() {
                token.push(ch);
                self.current_token_index += token.len();
                return Some(token);
            } else {
                self.current_token_index += token.len();
                return Some(token);
            }
        }
        self.current_token_index += token.len();
        return Some(token);
    }

    fn call_args(&mut self) -> Result<Box<[Box<str>]>> {
        match self.next_token() {
            None => return self.err("SYNTAX ERROR"),
            Some(token) => {
                if token != "(" {
                    return self.err("SYNTAX ERROR");
                }
            },
        }
        let mut result = Vec::new();
        loop {
            match self.next_token() {
                None => return self.err("SYNTAX ERROR"),
                Some(token) => {
                    if is_identifier_or_0(&token) {
                        result.push(token.into_boxed_str());
                    } else if token == ")" && result.is_empty() {
                        break;
                    }
                },
            }
            match self.next_token() {
                None => return self.err("SYNTAX ERROR"),
                Some(token) => {
                    if token == ")" {
                        break;
                    } else if token != "," {
                        return self.err("SYNTAX ERROR");
                    }
                },
            }
        }
        match self.next_token() {
            None => Ok(result.into_boxed_slice()),
            _ => self.err("SYNTAX ERROR"),
        }
    }

    fn err<T>(&self, msg: &str) -> Result<T> {
        err(Some(&(self.filename_index,self.current_line_number)), msg)
    }

    fn parse_uses(&mut self) -> Result<()> {
        loop {
            if !self.starts_with("USE") {
                return Ok(());
            }
            let token = match self.next_token() {
                None => return self.err("SYNTAX ERROR"),
                Some(token) => token,
            };
            if !is_identifier(&token) {
                return self.err("SYNTAX ERROR");
            }
            match self.next_token() {
                None => (),
                _ => return self.err("SYNTAX ERROR"),
            }
            self.uses.push(token.into_boxed_str());
            self.next_line(false)?;
        }
    }

    fn parse_subroutines(&mut self) -> Result<()> {
        loop {
            if !self.starts_with("SUBROUTINE") {
                return Ok(());
            }
            panic!("not implemented");
        }
    }

    fn parse_exports(&mut self) -> Result<bool> {
        if !self.starts_with("LIBRARY") {
            return Ok(false);
        }
        panic!("not implemented");
    }

    fn parse_program(&mut self) -> Result<bool> {
        if !self.starts_with("PROGRAM") {
            return Ok(false);
        }
        panic!("not implemented");
    }
}

impl<'a,R> RoutineParser<'a,R> {
}

struct ModuleResolver<'a> {
    module_routine_indexes: &'a HashMap<String,(usize,HashMap<String,(usize,bool)>)>,
    module_index: usize,
    routine_indexes: &'a HashMap<String,(usize,bool)>,
    uses: HashSet<String>,
}

struct RoutineResolver<'a> {
    module_resolver: &'a ModuleResolver<'a>,
    vars: HashMap<String,usize>,
    do_count: usize,
    do_stack: Vec<(String,usize)>,
    do_edges_count: usize,
    max_call_arg_count: usize,
    is_program: bool,
}

impl<'a> ModuleResolver<'a> {
    fn resolve(module: &mut Module, module_index: usize, module_routine_indexes: &HashMap<String,(usize,HashMap<String,(usize,bool)>)>) -> Result<()> {
        use std::ops::Deref;
        let mut uses = HashSet::new();
        for use_name in module.uses.iter() {
            uses.insert(String::from(use_name.deref()));
        }
        let routine_indexes = match module_routine_indexes.get(module.name.deref()) {
            Some(&(_,ref routine_indexes)) => routine_indexes,
            None => panic!(),
        };
        let module_resolver = ModuleResolver {
            module_routine_indexes,
            module_index,
            routine_indexes,
            uses,
        };
        for mut routine in module.routines.iter_mut() {
            RoutineResolver::resolve(&module_resolver, &mut routine, false)?;
        }
        for mut routine in module.program.iter_mut() {
            RoutineResolver::resolve(&module_resolver, &mut routine, true)?;
        }
        Ok(())
    }
}

impl<'a> RoutineResolver<'a> {
    fn resolve(module_resolver: &ModuleResolver, routine: &mut Routine, is_program: bool) -> Result<()> {
        let mut routine_resolver = RoutineResolver {
            module_resolver: module_resolver,
            vars: HashMap::new(),
            do_count: 0,
            do_stack: Vec::new(),
            do_edges_count: 0,
            max_call_arg_count: 0,
            is_program: is_program,
        };
        for parameter in routine.parameters.iter() {
            use std::ops::Deref;
            if routine_resolver.vars.contains_key(parameter.deref()) {
                return err(Some(&routine.src_location), &format!("DUPLICATE PARAMETER: {} IN ROUTINE: {}", &parameter, &routine.name));
            }
            let var_index = routine_resolver.vars.len();
            routine_resolver.vars.insert(String::from(parameter.deref()), var_index);
        }
        routine_resolver.resolve_statements(&mut routine.statements)?;
        routine.var_count = routine_resolver.vars.len();
        routine.do_edges_count = routine_resolver.do_edges_count;
        routine.max_call_arg_count = routine_resolver.max_call_arg_count;
        Ok(())
    }

    fn resolve_statements(&mut self, statements: &mut Box<[Statement]>) -> Result<()> {
        for mut statement in statements.iter_mut() {
            self.resolve_statement(&mut statement)?;
        }
        Ok(())
    }

    fn resolve_var(&mut self, src_location: &(usize,usize), var_name: &str) -> Result<usize> {
        if var_name == "0" {
            return err(Some(src_location), "SYNTAX ERROR");
        }
        match self.vars.get(var_name) {
            Some(var_index) => return Ok(*var_index),
            None => (),
        }
        let var_index = self.vars.len();
        self.vars.insert(String::from(var_name), var_index);
        Ok(var_index)
    }

    fn resolve_var_or_0(&mut self, var_name: &str) -> Option<usize> {
        if var_name == "0" {
            return None
        }
        match self.vars.get(var_name) {
            Some(var_index) => return Some(*var_index),
            None => (),
        }
        let var_index = self.vars.len();
        self.vars.insert(String::from(var_name), var_index);
        Some(var_index)
    }

    fn resolve_statement(&mut self, statement: &mut Statement) -> Result<()> {
        match statement {
            &mut Statement::LetEq {
                ref src_location,
                ref args,
                ref mut arg0_index,
                ref mut arg1_index,
            } => {
                use std::ops::Deref;
                *arg0_index = self.resolve_var(src_location, args[0].deref())?;
                *arg1_index = self.resolve_var_or_0(args[1].deref());
            },
            &mut Statement::LetAddEdge {
                ref src_location,
                ref args,
                ref mut arg0_index,
                ref mut arg1_index,
            } => {
                use std::ops::Deref;
                *arg0_index = self.resolve_var(src_location, args[0].deref())?;
                *arg1_index = self.resolve_var_or_0(args[1].deref());
            },
            &mut Statement::LetRemoveEdge {
                ref src_location,
                ref args,
                ref mut arg_indexes,
            } => {
                use std::ops::Deref;
                arg_indexes[0] = self.resolve_var(src_location, args[0].deref())?;
                arg_indexes[1] = self.resolve_var(src_location, args[1].deref())?;
            },
            &mut Statement::If(ref mut if_branches) => {
                for mut if_branch in if_branches.iter_mut() {
                    self.resolve_if_branch(&mut if_branch)?;
                }
            },
            &mut Statement::Call {
                ref src_location,
                ref module_name,
                ref routine_name,
                ref args,
                ref mut module_index,
                ref mut routine_index,
                ref mut arg_indexes,
            } => {
                match module_name {
                    &Some(ref name) => {
                        use std::ops::Deref;
                        if !self.module_resolver.uses.contains(name.deref()) {
                            return err(Some(src_location), &format!("NO USE DECLARATION FOR MODULE: {}", &name));
                        }
                        match self.module_resolver.module_routine_indexes.get(name.deref()) {
                            Some(&(index,ref routine_indexes)) => {
                                *module_index = index;
                                match routine_indexes.get(routine_name.deref()) {
                                    Some(&(index,exported)) => {
                                        if exported {
                                            *routine_index = index;
                                        } else {
                                            return err(Some(src_location), &format!("UNEXPORTED LIBRARY SUBROUTINE: {}", routine_name));
                                        }
                                    },
                                    None => {
                                        return err(Some(src_location), &format!("UNKNOWN LIBRARY SUBROUTINE: {}", routine_name));
                                    },
                                }
                            },
                            None => {
                                return err(Some(src_location), &format!("UNRESOLVED MODULE: {}", &name));
                            },
                        }
                    },
                    &None => {
                        use std::ops::Deref;
                        *module_index = self.module_resolver.module_index;
                        match self.module_resolver.routine_indexes.get(routine_name.deref()) {
                            Some(&(index,_)) => {
                                *routine_index = index;
                            },
                            None => {
                                return err(Some(src_location), &format!("UNDEFINED SUBROUTINE: {}", routine_name));
                            },
                        }
                    },
                }
                for i in 0 .. args.len() {
                    use std::ops::Deref;
                    arg_indexes[i] = self.resolve_var_or_0(args[i].deref());
                }
                if args.len() > self.max_call_arg_count {
                    self.max_call_arg_count = args.len();
                }
            },
            &mut Statement::Return {
                ref src_location,
            } => {
                if self.is_program {
                    return err(Some(src_location), "INVALID RETURN");
                }
            },
            &mut Statement::DoLoop {
                src_location: _,
                ref arg,
                ref mut statements,
                ref mut do_index,
            } => {
                use std::ops::Deref;
                *do_index = self.do_count;
                self.do_count += 1;
                self.do_stack.push((String::from(arg.deref()),*do_index));
                self.resolve_statements(statements)?;
                self.do_stack.pop();
            },
            &mut Statement::DoEdges {
                ref src_location,
                ref args,
                ref mut statements,
                ref mut arg_indexes,
                ref mut do_index,
                ref mut do_edges_index,
            } => {
                use std::ops::Deref;
                arg_indexes[0] = self.resolve_var(src_location, args[0].deref())?;
                arg_indexes[1] = self.resolve_var(src_location, args[1].deref())?;
                *do_index = self.do_count;
                self.do_count += 1;
                *do_edges_index = self.do_edges_count;
                self.do_edges_count += 1;
                self.do_stack.push((String::from(args[0].deref()),*do_index));
                self.resolve_statements(statements)?;
                self.do_stack.pop();
            },
            &mut Statement::Exit {
                ref src_location,
                ref arg,
                ref mut do_index,
            } => {
                *do_index = self.resolve_exit(src_location, arg)?;
            },
        }
        Ok(())
    }

    fn resolve_if_branch(&mut self, if_branch: &mut IfBranch) -> Result<()> {
        match if_branch {
            &mut IfBranch::Eq {
                ref src_location,
                ref args,
                ref mut arg_indexes,
                ref mut statements,
            } => {
                use std::ops::Deref;
                arg_indexes[0] = self.resolve_var(src_location, args[0].deref())?;
                arg_indexes[1] = self.resolve_var(src_location, args[1].deref())?;
                self.resolve_statements(statements)?;
            },
            &mut IfBranch::Edges {
                ref src_location,
                ref args,
                ref mut arg_indexes,
                ref mut statements,
            } => {
                use std::ops::Deref;
                arg_indexes[0] = self.resolve_var(src_location, args[0].deref())?;
                arg_indexes[1] = self.resolve_var(src_location, args[1].deref())?;
                self.resolve_statements(statements)?;
            },
            &mut IfBranch::Else {
                src_location: _,
                ref mut statements,
            } => {
                self.resolve_statements(statements)?;
            },
        }
        Ok(())
    }

    fn resolve_exit(&mut self, src_location: &(usize,usize), arg: &Box<str>) -> Result<usize> {
        for i in 0 .. self.do_stack.len() {
            let (ref do_arg,ref do_index) = self.do_stack[self.do_stack.len()-1-i];
            use std::ops::Deref;
            if do_arg == arg.deref() {
                return Ok(*do_index);
            }
        }
        err(Some(src_location), &format!("INVALID EXIT: {}", arg))
    }
}
