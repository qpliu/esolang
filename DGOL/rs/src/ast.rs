use std::collections::{HashMap,HashSet};
use std::io::{Error,ErrorKind,Result,Read};

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
    Edge {
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

fn trace(&(filename_index,line_number): &(usize,usize), msg: &str) {
    if false {
        use std::env::args;
        println!("{}:{} {}", args().nth(filename_index+1).unwrap(), line_number, msg);
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
                ref src_location,
                ref arg0_index,
                ref arg1_index,
                ..
            } => {
                trace(src_location, "LET=");
                let node = match arg1_index {
                    &Some(index) => scope.get_var(index, node_pool),
                    &None => scope.new_node(node_pool),
                };
                scope.set_var(*arg0_index, &node);
                Next::Fallthrough
            },
            &Statement::LetAddEdge {
                ref src_location,
                ref arg0_index,
                ref arg1_index,
                ..
            } => {
                trace(src_location, "LET>");
                let node = match arg1_index {
                    &Some(index) => scope.get_var(index, node_pool),
                    &None => scope.new_node(node_pool),
                };
                scope.get_var(*arg0_index, node_pool).add_edge(&node);
                Next::Fallthrough
            },
            &Statement::LetRemoveEdge {
                ref src_location,
                ref arg_indexes,
                ..
            } => {
                trace(src_location, "LET<");
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
                ref src_location,
                ref module_index,
                ref routine_index,
                ref arg_indexes,
                ..
            } => {
                trace(src_location, "CALL");
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
                ref src_location,
            } => {
                trace(src_location, "RETURN");
                Next::Return
            },
            &Statement::DoLoop {
                ref src_location,
                ref statements,
                ref do_index,
                ..
            } => {
                trace(src_location, "DO");
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
                ref src_location,
                ref statements,
                ref arg_indexes,
                ref do_index,
                ref do_edges_index,
                ..
            } => {
                trace(src_location, "DO<");
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
                ref src_location,
                ref do_index,
                ..
            } => {
                trace(src_location, "EXIT");
                Next::Exit(*do_index)
            },
        }
    }
}

impl IfBranch {
    fn execute(&self, program: &interp::Program, scope: &mut Scope, node_pool: &mut NodePool) -> Option<Next> {
        match self {
            &IfBranch::Eq {
                ref src_location,
                ref arg_indexes,
                ref statements,
                ..
            } => {
                trace(src_location, "IF=");
                let arg0 = scope.get_var(arg_indexes[0], node_pool);
                let arg1 = scope.get_var(arg_indexes[1], node_pool);
                if arg0 == arg1 {
                    return Some(execute_statements(statements, program, scope, node_pool))
                }
            },
            &IfBranch::Edge {
                ref src_location,
                ref arg_indexes,
                ref statements,
                ..
            } => {
                trace(src_location, "IF>");
                let arg0 = scope.get_var(arg_indexes[0], node_pool);
                let arg1 = scope.get_var(arg_indexes[1], node_pool);
                if arg0.has_edge(&arg1) {
                    return Some(execute_statements(statements, program, scope, node_pool))
                }
            },
            &IfBranch::Else {
                ref src_location,
                ref statements,
            } => {
                trace(src_location, "ELSE");
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

struct ModuleParser<R> {
    filename_index: usize,
    reader: R,
    current_line: String,
    current_line_number: usize,
    current_token_index: usize,
    seen_eof: bool,

    name: String,
    uses: Vec<Box<str>>,
    routines: Vec<Routine>,
    exports: Vec<Box<str>>,
    program: Option<Routine>,
}

impl<R:Read> ModuleParser<R> {
    pub fn parse(filename_index: usize, r: R) -> Result<Module> {
        let mut module_parser = ModuleParser {
            filename_index: filename_index,
            reader: r,
            current_line: String::new(),
            current_line_number: 0,
            current_token_index: 0,
            seen_eof: false,

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
        module_parser.next_line(true)?;
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
                } else if !ch.is_whitespace() {
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

    fn next_token_is(&mut self, token: &str) -> bool {
        let start = self.current_token_index;
        match self.next_token() {
            None => false,
            Some(tok) => {
                if tok == token {
                    true
                } else {
                    self.current_token_index = start;
                    false
                }
            },
        }
    }

    fn next_token_must_be(&mut self, token: &str) -> Result<()> {
        if self.next_token_is(token) {
            Ok(())
        } else {
            self.err("SYNTAX ERROR")
        }
    }

    fn next_identifier(&mut self) -> Result<String> {
        match self.next_token() {
            None => self.err("SYNTAX ERROR"),
            Some(token) => {
                if !is_identifier(&token) {
                    self.err("SYNTAX ERROR")
                } else {
                    Ok(token)
                }
            },
        }
    }

    fn next_identifier_or_0(&mut self) -> Result<String> {
        match self.next_token() {
            None => self.err("SYNTAX ERROR"),
            Some(token) => {
                if !is_identifier_or_0(&token) {
                    self.err("SYNTAX ERROR")
                } else {
                    Ok(token)
                }
            },
        }
    }

    fn no_more_tokens(&mut self) -> Result<()> {
        if self.next_token().is_some() {
            return self.err("SYNTAX ERROR");
        }
        Ok(())
    }

    fn is_end(&mut self, name: &str) -> Result<()> {
        if !self.starts_with("END") {
            return self.err("SYNTAX ERROR");
        }
        self.next_token_must_be(name)?;
        self.no_more_tokens()?;
        Ok(())
    }

    fn call_args(&mut self) -> Result<Box<[Box<str>]>> {
        self.next_token_must_be("(")?;
        let mut result = Vec::new();
        if !self.next_token_is(")") {
            loop {
                result.push(self.next_identifier_or_0()?.into_boxed_str());
                if self.next_token_is(")") {
                    break;
                }
                self.next_token_must_be(",")?;
            }
        }
        self.no_more_tokens()?;
        Ok(result.into_boxed_slice())
    }

    fn err<T>(&self, msg: &str) -> Result<T> {
        err(Some(&(self.filename_index,self.current_line_number)), msg)
    }

    fn parse_uses(&mut self) -> Result<()> {
        loop {
            if !self.starts_with("USE") {
                return Ok(());
            }
            let token = self.next_identifier()?;
            self.no_more_tokens()?;
            self.uses.push(token.into_boxed_str());
            self.next_line(false)?;
        }
    }

    fn parse_subroutines(&mut self) -> Result<()> {
        loop {
            match self.parse_subroutine()? {
                None => return Ok(()),
                Some(routine) => {
                    self.routines.push(routine);
                    self.next_line(false)?;
                },
            }
        }
    }

    fn parse_exports(&mut self) -> Result<bool> {
        if !self.starts_with("LIBRARY") {
            return Ok(false);
        }
        let name = self.next_identifier()?;
        self.no_more_tokens()?;
        self.next_line(false)?;
        loop {
            if !self.starts_with("SUBROUTINE") {
                break;
            }
            let export = self.next_identifier()?;
            self.no_more_tokens()?;
            let mut valid_export = false;
            for mut routine in self.routines.iter_mut() {
                use std::ops::Deref;
                if export == routine.name.deref() {
                    routine.exported = true;
                    valid_export = true;
                    break;
                }
            }
            if !valid_export {
                return self.err(&format!("UNDEFINED SUBROUTINE: {}", export));
            }
            self.exports.push(export.into_boxed_str());
            self.next_line(false)?;
        }
        self.is_end(name.as_str())?;
        Ok(true)
    }

    fn parse_program(&mut self) -> Result<bool> {
        if !self.starts_with("PROGRAM") {
            return Ok(false);
        }
        let name = self.next_identifier()?;
        self.name.push_str(&name);
        self.no_more_tokens()?;
        let parameters = Vec::new().into_boxed_slice();
        self.program = Some(self.parse_routine_body(name, parameters)?);
        Ok(true)
    }

    fn parse_subroutine(&mut self) -> Result<Option<Routine>> {
        if !self.starts_with("SUBROUTINE") {
            return Ok(None);
        }
        let name = self.next_identifier()?;
        let parameters = self.call_args()?;
        Ok(Some(self.parse_routine_body(name, parameters)?))
    }

    fn parse_routine_body(&mut self, name: String, parameters: Box<[Box<str>]>) -> Result<Routine> {
        let src_location = (self.filename_index,self.current_line_number);
        let statements = self.parse_statements()?;
        self.is_end(name.as_str())?;
        Ok(Routine {
                src_location,
                name: name.into_boxed_str(),
                parameters,
                statements: statements.into_boxed_slice(),
                exported: false,

                var_count: 0,
                do_edges_count: 0,
                max_call_arg_count: 0,
            })
    }

    fn parse_statements(&mut self) -> Result<Vec<Statement>> {
        let mut statements = Vec::new();
        loop {
            self.next_line(false)?;
            match self.parse_statement()? {
                None => return Ok(statements),
                Some(statement) => statements.push(statement),
            }
        }
    }

    fn parse_statement(&mut self) -> Result<Option<Statement>> {
        let src_location = (self.filename_index,self.current_line_number);
        if self.starts_with("LET") {
            let arg0 = self.next_identifier()?;
            if self.next_token_is("=") {
                let arg1 = self.next_identifier_or_0()?;
                self.no_more_tokens()?;
                Ok(Some(Statement::LetEq {
                            src_location,
                            args: [arg0.into_boxed_str(), arg1.into_boxed_str()],
                            arg0_index: 0,
                            arg1_index: None,
                        }))
            } else if self.next_token_is(">") {
                let arg1 = self.next_identifier_or_0()?;
                self.no_more_tokens()?;
                Ok(Some(Statement::LetAddEdge {
                            src_location,
                            args: [arg0.into_boxed_str(), arg1.into_boxed_str()],
                            arg0_index: 0,
                            arg1_index: None,
                        }))
            } else if self.next_token_is("<") {
                let arg1 = self.next_identifier_or_0()?;
                self.no_more_tokens()?;
                Ok(Some(Statement::LetRemoveEdge {
                            src_location,
                            args: [arg0.into_boxed_str(), arg1.into_boxed_str()],
                            arg_indexes: [0,0],
                        }))
            } else {
                self.err("SYNTAX ERROR")
            }
        } else if self.starts_with("IF") {
            let mut if_branches = Vec::new();
            loop {
                let if_branch = self.parse_if_branch(if_branches.is_empty())?;
                match if_branch {
                    None => break,
                    Some(branch) => {
                        let is_else = match &branch {
                            &IfBranch::Else { .. } => true,
                            _ => false,
                        };
                        if_branches.push(branch);
                        if is_else {
                            break;
                        }
                    },
                }
            }
            self.is_end("IF")?;
            Ok(Some(Statement::If(if_branches.into_boxed_slice())))
        } else if self.starts_with("CALL") {
            let first_name = self.next_identifier()?;
            let (module_name,routine_name) = if self.next_token_is(".") {
                let second_name = self.next_identifier()?;
                (Some(first_name.into_boxed_str()),second_name.into_boxed_str())
            } else {
                (None,first_name.into_boxed_str())
            };
            let args = self.call_args()?;
            Ok(Some(Statement::Call {
                        src_location,
                        module_name,
                        routine_name,
                        args,
                        module_index: 0,
                        routine_index: 0,
                        arg_indexes: Vec::new().into_boxed_slice(),
                    }))
        } else if self.starts_with("RETURN") {
            self.no_more_tokens()?;
            Ok(Some(Statement::Return { src_location }))
        } else if self.starts_with("DO") {
            let arg = self.next_identifier()?;
            if self.next_token_is("<") {
                let arg1 = self.next_identifier()?;
                self.no_more_tokens()?;
                let statements = self.parse_statements()?;
                self.is_end("DO")?;
                Ok(Some(Statement::DoEdges {
                            src_location,
                            args: [arg.into_boxed_str(), arg1.into_boxed_str()],
                            statements: statements.into_boxed_slice(),
                            arg_indexes: [0,0],
                            do_index: 0,
                            do_edges_index: 0,
                        }))
            } else {
                self.no_more_tokens()?;
                let statements = self.parse_statements()?;
                self.is_end("DO")?;
                Ok(Some(Statement::DoLoop {
                            src_location: src_location,
                            arg: arg.into_boxed_str(),
                            statements: statements.into_boxed_slice(),
                            do_index: 0,
                        }))
            }
        } else if self.starts_with("EXIT") {
            let arg = self.next_identifier()?;
            self.no_more_tokens()?;
            Ok(Some(Statement::Exit {
                        src_location,
                        arg: arg.into_boxed_str(),
                        do_index: 0,
                    }))
        } else {
            Ok(None)
        }
    }

    fn parse_if_branch(&mut self, is_first_branch: bool) -> Result<Option<IfBranch>> {
        let src_location = (self.filename_index,self.current_line_number);
        if is_first_branch {
            if !self.starts_with("IF") {
                return self.err("SYNTAX ERROR");
            }
        } else {
            if self.starts_with("ELSEIF") {
            } else if self.starts_with("ELSE") {
                self.no_more_tokens()?;
                let statements = self.parse_statements()?;
                return Ok(Some(IfBranch::Else {
                            src_location: src_location,
                            statements: statements.into_boxed_slice(),
                        }));
            } else {
                return Ok(None);
            }
        }
        let arg0 = self.next_identifier()?;
        if self.next_token_is("=") {
            let arg1 = self.next_identifier()?;
            self.no_more_tokens()?;
            let statements = self.parse_statements()?;
            Ok(Some(IfBranch::Eq {
                        src_location,
                        args: [arg0.into_boxed_str(), arg1.into_boxed_str()],
                        arg_indexes: [0,0],
                        statements: statements.into_boxed_slice(),
                    }))
        } else {
            self.next_token_must_be(">")?;
            let arg1 = self.next_identifier()?;
            self.no_more_tokens()?;
            let statements = self.parse_statements()?;
            Ok(Some(IfBranch::Edge {
                        src_location,
                        args: [arg0.into_boxed_str(), arg1.into_boxed_str()],
                        arg_indexes: [0,0],
                        statements: statements.into_boxed_slice(),
                    }))
        }
    }
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
                let mut vec = Vec::new();
                for arg in args.iter() {
                    use std::ops::Deref;
                    vec.push(self.resolve_var_or_0(arg.deref()));
                }
                *arg_indexes = vec.into_boxed_slice();
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
                ref arg,
                ref mut statements,
                ref mut do_index,
                ..
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
            &mut IfBranch::Edge {
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
                ref mut statements,
                ..
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
