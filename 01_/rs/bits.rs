pub trait Bits: Clone {
    fn from_vec(vec: &[bool]) -> Self;

    fn from_reader(reader: ~Reader) -> Self;

    fn nil() -> Self;

    fn eval(&mut self) -> Option<(bool,Self)>;

    fn from_file(path: &Path) -> Self {
        use std::io::File;
        Bits::from_reader(~File::open(path))
    }

    fn write(&self, writer: &mut Writer) {
        let mut bits = self.clone();
        let mut byte = 0u8;
        let mut bit = 128u8;
        loop {
            match bits.eval() {
                None => { return; } // drop trailing partial byte
                Some((true,ref rest)) => {
                    byte |= bit;
                    bits = rest.clone();
                },
                Some((false,ref rest)) => {
                    bits = rest.clone();
                },
            }
            bit >>= 1;
            if bit == 0 {
                writer.write_u8(byte).ok();
                byte = 0;
                bit = 128;
            }
        }
    }

    fn bits_to_str(&self) -> ~str {
        let mut str = ~"";
        let mut b = self.clone();
        loop {
            match b.eval() {
                None => {
                    return str;
                },
                Some((true,ref rest)) => {
                    str.push_char('1');
                    b = rest.clone();
                },
                Some((false,ref rest)) => {
                    str.push_char('0');
                    b = rest.clone();
                },
            }
        }
    }
}
