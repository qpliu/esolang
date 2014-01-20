use location::Location;

pub struct Error {
    location: Location,
    message: ~str,
}

impl Error {
    pub fn new(location: Location, message: ~str) -> Error {
        Error { location: location.clone(), message: message }
    }

    pub fn new_range(start_location: Location, end_location: Location, message: ~str) -> Error {
        Error { location: start_location + end_location, message: message }
    }
}

impl ToStr for Error {
    fn to_str(&self) -> ~str {
        self.location.to_str() + ": " + self.message
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use super::Error;
    use location::Location;

    #[test]
    fn test_to_str() {
        let file_name = Rc::new(~"test");
        let location1 = Location {
            file_name: file_name.clone(),
            start_line: 1,
            start_column: 2,
            end_line: 1,
            end_column: 4,
        };
        assert!("test:1:3: 1:4: oops" == Error::new(location1.clone(), ~"oops").to_str());
        let location2 = Location {
            file_name: file_name.clone(),
            start_line: 3,
            start_column: 5,
            end_line: 3,
            end_column: 8,
        };
        assert!("test:1:3: 3:8: oops" == Error::new_range(location1.clone(), location2.clone(), ~"oops").to_str());
    }
}
