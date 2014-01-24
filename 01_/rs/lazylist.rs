// Non-concurrent acyclic lazy lists.
// As I don't know what I'm doing, I'm probably doing it wrong.
// This was written with rustc 0.10-pre (fa91446 2014-01-16 07:06:58 -0800).
// rustc 0.9 (or older) will not work.

use std::cell::RefCell;
use std::rc::Rc;
use std::util;

pub struct LazyList<T> {
    priv list: Rc<RefCell<Element<T>>>,
}

pub struct LazyListIterator<T> {
    priv list: LazyList<T>,
}

enum Element<T> {
    Unevaluated(~Evaluator:<T>),
    Evaluated(Option<(T,LazyList<T>)>),
    Concat(LazyList<T>,LazyList<T>),
    Placeholder,
}

pub trait Evaluator<T> {
    fn eval(~self) -> Option<(T,~Evaluator:<T>)>;
}

impl<T:Pod+Clone> LazyList<T> {
    pub fn new(element: ~Evaluator:<T>) -> LazyList<T> {
        LazyList { list: Rc::new(RefCell::new(Unevaluated(element))) }
    }

    pub fn concat(list1: LazyList<T>, list2: LazyList<T>) -> LazyList<T> {
        LazyList { list: Rc::new(RefCell::new(Concat(list1,list2))) }
    }

    pub fn from_iter(iter: ~Iterator:<T>) -> LazyList<T> {
        struct FromIter<T> { iter: ~Iterator:<T> }
        impl<T> Evaluator<T> for FromIter<T> {
            fn eval(mut ~self) -> Option<(T,~Evaluator:<T>)> {
                match self.iter.next() {
                    None => None,
                    Some(t) => Some((t,self as ~Evaluator:<T>)),
                }
            }
        }
        LazyList::new(~FromIter { iter: iter } as ~Evaluator:<T>)
    }

    pub fn eval(&self) -> Option<(T,LazyList<T>)> {
        self.list.borrow().with_mut(|element: &mut Element<T>| -> Option<(T,LazyList<T>)> {
            match *element {
                Evaluated(ref result) => { return result.clone(); },
                _ => (),
            }
            let mut temp = Placeholder;
            util::swap(&mut temp, element);
            let result = match temp {
                Unevaluated(eval) => {
                    match eval.eval() {
                        None => None,
                        Some((t,next)) => Some((t,LazyList::new(next))),
                    }
                },
                Concat(head,tail) => {
                    match head.eval() {
                        Some((t,rest)) => Some((t,LazyList::concat(rest,tail))),
                        None => {
                            match tail.eval() {
                                None => None,
                                Some((t,rest)) => Some((t,rest.clone())),
                            }
                        },
                    }
                },
                _ => unreachable!(),
            };
            *element = Evaluated(result.clone());
            result
        })
    }

    pub fn iter(&self) -> LazyListIterator<T> {
        LazyListIterator { list: self.clone() }
    }
}

impl<T> LazyList<T> {
    pub fn nil() -> LazyList<T> {
        LazyList { list: Rc::new(RefCell::new(Evaluated(None))) }
    }
}

impl<T> Clone for LazyList<T> {
    fn clone(&self) -> LazyList<T> {
        LazyList { list: self.list.clone() }
    }
}

impl<T:Pod+Clone> Add<LazyList<T>,LazyList<T>> for LazyList<T> {
    fn add(&self, other: &LazyList<T>) -> LazyList<T> {
        LazyList::concat(self.clone(), other.clone())
    }
}

impl<T:Pod+Clone> Iterator<T> for LazyListIterator<T> {
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
    fn test_from_iter() {
        let l = LazyList::from_iter(~(~[1, 2, 3]).move_iter());
        assert!([1, 2, 3] == l.iter().to_owned_vec());
    }

    #[test]
    fn test_nil() {
        let l : LazyList<bool> = LazyList::nil();
        assert!([] == l.iter().to_owned_vec());
    }

    #[test]
    fn test_add() {
        let l = LazyList::from_iter(~(~[1, 2, 3]).move_iter());
        assert!([1, 2, 3, 1, 2, 3] == (l + l).iter().to_owned_vec());
    }
}
