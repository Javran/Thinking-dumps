use kmp::kmp_find;
use std::cmp::Ordering;

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

/// Tests whether `ws` is a (consecutive) sublist of `ys`.
/// `ws` for "word string" and `ts` for "text string".
fn is_sublist_of<T: PartialEq>(ws: &[T], ts: &[T]) -> bool {
    kmp_find(ws, ts).is_some()
}

pub fn sublist<T: PartialEq>(xs: &[T], ys: &[T]) -> Comparison {
    match xs.len().cmp(&ys.len()) {
        Ordering::Equal if xs == ys => Comparison::Equal,
        Ordering::Less if is_sublist_of(xs, ys) => Comparison::Sublist,
        Ordering::Greater if is_sublist_of(ys, xs) => Comparison::Superlist,
        _ => Comparison::Unequal,
    }
}
