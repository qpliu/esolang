mod ast;
mod bits;
mod bits1;
mod error;
mod interp;
mod interp1;
mod location;
mod parse1;
mod symbol;
mod token;

fn get_main(args: &[~str]) -> ~str {
    assert!(args.len() > 0);
    for i in range(1, args.len()-1) {
        if "-" == args[i] {
            return args[i+1].to_str();
        }
    }
    let mut path = Path::new(args[0].to_str());
    path.set_extension("");
    return path.filename_str().unwrap_or("main").to_str();
}

fn get_src_files(args: &[~str]) -> ~[Path] {
    assert!(args.len() > 0);
    for i in range(1, args.len()) {
        if "-" == args[i] {
            return args.slice_to(i).map(|arg| Path::new(arg.to_str()));
        }
    }
    return args.map(|arg| Path::new(arg.to_str()));
}

fn get_arg_files(args: &[~str]) -> ~[Path] {
    assert!(args.len() > 0);
    for i in range(1, args.len()-1) {
        if "-" == args[i] {
            return args.slice_from(i+2).map(|arg| Path::new(arg.to_str()));
        }
    }
    ~[]
}

#[cfg(not(test))]
fn error(msg: &[~str]) -> ! {
    use std::io;
    use std::libc;
    for line in msg.iter() {
        io::stderr().write_line(line.to_str());
    }
    unsafe { libc::exit(1) }
}

#[cfg(not(test))]
fn main() {
    use std::io;
    use std::os;
    use ast::Ast;
    use bits::Bits;
    use bits1::Bits1;
    use interp::Interp;
    use interp1::Interp1;

    let mut args = os::args();
    let name = args.shift();
    if args.len() < 1 {
        error([format!("usage: {} FILENAME ... [- FUNCTION [FILENAME ...]]", name)]);
    }
    let mut interp : Interp1 = Interp::new();
    let ast : Ast<Bits1> = match Ast::parse(get_src_files(args), &|bits| interp.constant(bits)) {
        Ok(ast) => ast,
        Err(errors) => error(errors.map(|e| e.to_str())),
    };
    let main_name = get_main(args);
    let main_index = match ast.lookup_index_by_str(main_name) {
        None => error([format!("No such function `{}`", main_name)]),
        Some(index) => index,
    };
    let main_arity = ast.lookup(main_index).arity();
    let mut arg_files = get_arg_files(args);
    while arg_files.len() > main_arity {
        arg_files.pop();
    }
    assert!(arg_files.len() <= main_arity);
    let mut args = arg_files.map(|path| Bits::from_file(path));
    assert!(args.len() <= main_arity);
    if args.len() < main_arity {
        args.push(Bits::from_reader(~io::stdin()));
    }
    while args.len() < main_arity {
        args.push(Bits::nil());
    }
    
    assert!(args.len() == main_arity);
    interp.run(ast, main_index, args, &mut io::stdout());
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_get_main() {
        use super::get_main;

        let args = ~[~"test.01_", ~"-", ~"m"];
        assert!("m" == get_main(args));

        let args = ~[~"test.01_", ~"a.01_", ~"b.01_"];
        assert!("test" == get_main(args));
    }

    #[test]
    fn test_get_src_files() {
        use super::get_src_files;

        let args = ~[~"test.01_", ~"-", ~"m"];
        assert!([~"test.01_"] == get_src_files(args).map(|path| path.filename_str().unwrap().to_str()));

        let args = ~[~"test.01_", ~"a.01_", ~"b.01_"];
        assert!([~"test.01_", ~"a.01_", ~"b.01_"] == get_src_files(args).map(|path| path.filename_str().unwrap().to_str()));
    }

    #[test]
    fn test_get_arg_files() {
        use super::get_arg_files;

        let args = ~[~"test.01_", ~"-", ~"m"];
        assert!([] == get_arg_files(args).map(|path| path.filename_str().unwrap().to_str()));

        let args = ~[~"test.01_", ~"a.01_", ~"b.01_"];
        assert!([] == get_arg_files(args).map(|path| path.filename_str().unwrap().to_str()));

        let args = ~[~"test.01_", ~"a.01_", ~"b.01_", ~"-", ~"main", ~"arg"];
        assert!([~"arg"] == get_arg_files(args).map(|path| path.filename_str().unwrap().to_str()));
    }
}
