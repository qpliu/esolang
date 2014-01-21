// Non-concurrent acyclic lazy lists.
// As I don't know what I'm doing, I'm probably doing it wrong.
// This was written with rustc 0.10-pre (fa91446 2014-01-16 07:06:58 -0800).
// rustc 0.9 (or older) will not work.

use std::cell::RefCell;
use std::rc::Rc;
use std::util;

pub struct LazyList<'a,T> {
    priv list: Rc<RefCell<ListElement<'a,T>>>,
}

pub struct LazyListIterator<'a,T> {
    priv list: LazyList<'a,T>,
}

enum ListElement<'a,T> {
    Unevaluated(&'a mut Iterator<T>),
    Concat(LazyList<'a,T>,LazyList<'a,T>),
    Cons(T,LazyList<'a,T>),
    Nil,
}

impl<'a,T:Pod> LazyList<'a,T> {
    pub fn new(iter: &'a mut Iterator<T>) -> LazyList<'a,T> {
        LazyList { list: Rc::new(RefCell::new(Unevaluated(iter))) }
    }

    pub fn eval(&self) -> Option<(T,LazyList<'a,T>)> {
        self.list.borrow().with_mut(|element: &mut ListElement<'a,T>| -> Option<(T,LazyList<'a,T>)> {
            enum MatchResult<'a,T> {
                Ready(Option<(T,LazyList<'a,T>)>),
                EndList,
                ContinueIter(T),
                ContinueConcat(T,LazyList<'a,T>,LazyList<'a,T>),
                EndConcat(T,LazyList<'a,T>),
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

    pub fn iter(&self) -> LazyListIterator<'a,T> {
        LazyListIterator { list: self.clone() }
    }
}

impl<'a,T> LazyList<'a,T> {
    pub fn nil() -> LazyList<'a,T> {
        LazyList { list: Rc::new(RefCell::new(Nil)) }
    }
}

impl<'a,T> Clone for LazyList<'a,T> {
    fn clone(&self) -> LazyList<'a,T> {
        LazyList { list: self.list.clone() }
    }
}

impl<'a,T:Pod> Add<LazyList<'a,T>,LazyList<'a,T>> for LazyList<'a,T> {
    fn add(&self, other: &LazyList<'a,T>) -> LazyList<'a,T> {
        LazyList { list: Rc::new(RefCell::new(Concat(self.clone(), other.clone()))) }
    }
}

impl<'a,T:Pod> Iterator<T> for LazyListIterator<'a,T> {
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
        let o = [1, 2, 3];
        let mut iter = o.iter().map(|&i| i);
        let l = LazyList::new(&mut iter);
        assert!([1, 2, 3] == l.iter().to_owned_vec());
    }

    #[test]
    fn test_nil() {
        let l : LazyList<bool> = LazyList::nil();
        assert!([] == l.iter().to_owned_vec());
    }

    #[test]
    fn test_add() {
        let o = [1, 2, 3];
        let mut iter = o.iter().map(|&i| i);
        let l = LazyList::new(&mut iter);
        assert!([1, 2, 3, 1, 2, 3] == (l + l).iter().to_owned_vec());
    }
}
