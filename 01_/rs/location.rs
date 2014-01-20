use std::rc::Rc;

pub struct Location {
    file_name: Rc<~str>,
    start_line: uint,
    start_column: uint,
    end_line: uint,
    end_column: uint,
}

impl Clone for Location {
    fn clone(&self) -> Location {
        Location { file_name: self.file_name.clone(), ..*self }
    }
}

impl ToStr for Location {
    fn to_str(&self) -> ~str {
        format!("{}:{}:{}: {}:{}", *self.file_name.borrow(), self.start_line, self.start_column+1, self.end_line, self.end_column)
    }
}

impl Add<Location,Location> for Location {
    fn add(&self, other: &Location) -> Location {
        let mut result = self.clone();
        if self.file_name == other.file_name {
            result.end_line = other.end_line;
            result.end_column = other.end_column;
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;
    use super::Location;

    #[test]
    fn test_to_str() {
        let location = Location {
            file_name: Rc::new(~"test"),
            start_line: 1,
            start_column: 2,
            end_line: 1,
            end_column: 4,
        };
        assert!("test:1:3: 1:4" == location.to_str());
    }

    #[test]
    fn test_add() {
        let file_name = Rc::new(~"test");
        let location1 = Location {
            file_name: file_name.clone(),
            start_line: 1,
            start_column: 2,
            end_line: 1,
            end_column: 4,
        };
        let location2 = Location {
            file_name: file_name.clone(),
            start_line: 1,
            start_column: 5,
            end_line: 2,
            end_column: 9,
        };
        assert!("test:1:3: 2:9" == (location1+location2).to_str());
        let location3 = Location {
            file_name: Rc::new(~"test3"),
            start_line: 40,
            start_column: 5,
            end_line: 45,
            end_column: 9,
        };
        assert!("test:1:3: 1:4" == (location1+location3).to_str());
    }
}
