// Non-concurrent acyclic lazy lists.
// As I don't know what I'm doing, I'm probably doing it wrong.
// This was written with rustc 0.10-pre (fa91446 2014-01-16 07:06:58 -0800).
// rustc 0.9 (or older) will not work.

use std::cell::RefCell;
use std::rc::Rc;
use std::util;

pub struct LazyList<T> {
    priv list: Rc<RefCell<ListElement<T>>>,
}

pub struct LazyListIterator<T> {
    priv list: LazyList<T>,
}

enum ListElement<T> {
    Unevaluated(~Iterator:<T>),
    Concat(LazyList<T>,LazyList<T>),
    Cons(T,LazyList<T>),
    Nil,
}

impl<T:Pod> LazyList<T> {
    pub fn new(iter: ~Iterator:<T>) -> LazyList<T> {
        LazyList { list: Rc::new(RefCell::new(Unevaluated(iter))) }
    }

    pub fn eval(&self) -> Option<(T,LazyList<T>)> {
        self.list.borrow().with_mut(|element: &mut ListElement<T>| -> Option<(T,LazyList<T>)> {
            enum MatchResult<T> {
                Ready(Option<(T,LazyList<T>)>),
                EndList,
                ContinueIter(T),
                ContinueConcat(T,LazyList<T>,LazyList<T>),
                EndConcat(T,LazyList<T>),
            }
            let result = match *element {
                Nil => Ready(None),
                Cons(item,ref rest) => Ready(Some((item,rest.clone()))),
                Unevaluated(ref mut iter) => match iter.next() {
                    None => EndList,
                    Some(item) => ContinueIter(item),
                },
                Concat(ref head,ref tail) => {
                    match head.eval() {
                        Some((item,ref rest)) => ContinueConcat(item,rest.clone(),tail.clone()),
                        None => {
                            match tail.eval() {
                                None => EndList,
                                Some((item,ref rest)) => EndConcat(item,rest.clone()),
                            }
                        }
                    }
                },
            };
            match result {
                Ready(r) => r,
                EndList => {
                    util::replace(element, Nil);
                    None
                },
                ContinueIter(item) => {
                    let new_tail_element = util::replace(element, Nil);
                    let new_tail = LazyList { list: Rc::new(RefCell::new(new_tail_element)) };
                    util::replace(element, Cons(item,new_tail.clone()));
                    Some((item,new_tail))
                },
                ContinueConcat(item,ref head,ref tail) => {
                    let new_tail = LazyList { list: Rc::new(RefCell::new(Concat(head.clone(), tail.clone()))) };
                    util::replace(element, Cons(item,new_tail.clone()));
                    Some((item,new_tail))
                },
                EndConcat(item,ref tail) => {
                    util::replace(element, Cons(item,tail.clone()));
                    Some((item,tail.clone()))
                },
            }
        })
    }

    pub fn iter(&self) -> LazyListIterator<T> {
        LazyListIterator { list: self.clone() }
    }
}

impl<T> LazyList<T> {
    pub fn nil() -> LazyList<T> {
        LazyList { list: Rc::new(RefCell::new(Nil)) }
    }
}

impl<T> Clone for LazyList<T> {
    fn clone(&self) -> LazyList<T> {
        LazyList { list: self.list.clone() }
    }
}

impl<T:Pod> Add<LazyList<T>,LazyList<T>> for LazyList<T> {
    fn add(&self, other: &LazyList<T>) -> LazyList<T> {
        LazyList { list: Rc::new(RefCell::new(Concat(self.clone(), other.clone()))) }
    }
}

impl<T:Pod> Iterator<T> for LazyListIterator<T> {
    fn next(&mut self) -> Option<T> {
        match self.list.eval() {
            None => None,
            Some((e,rest)) => {
                self.list = rest.clone();
                Some(e)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::LazyList;

    #[test]
    fn test_new() {
        let l = LazyList::new(~(~[1, 2, 3]).move_iter());
        assert!([1, 2, 3] == l.iter().to_owned_vec());
    }

    #[test]
    fn test_nil() {
        let l : LazyList<bool> = LazyList::nil();
        assert!([] == l.iter().to_owned_vec());
    }

    #[test]
    fn test_add() {
        let l = LazyList::new(~(~[1, 2, 3]).move_iter());
        assert!([1, 2, 3, 1, 2, 3] == (l + l).iter().to_owned_vec());
    }
}
