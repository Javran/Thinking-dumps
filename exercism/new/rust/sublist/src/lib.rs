use std::cmp::Ordering;

#[derive(Debug, PartialEq)]
pub enum Comparison {
    Equal,
    Sublist,
    Superlist,
    Unequal,
}

/// Tests whether `needle` is a (consecutive) sublist of `haystack`.
fn is_sublist_of<T: PartialEq>(needle: &[T], haystack: &[T]) -> bool {
    if needle.is_empty() {
        return true;
    }
    haystack.windows(needle.len()).any(|chunk| needle == chunk)
}

pub fn sublist<T: PartialEq>(xs: &[T], ys: &[T]) -> Comparison {
    match xs.len().cmp(&ys.len()) {
        Ordering::Equal if xs == ys => Comparison::Equal,
        Ordering::Less if is_sublist_of(xs, ys) => Comparison::Sublist,
        Ordering::Greater if is_sublist_of(ys, xs) => Comparison::Superlist,
        _ => Comparison::Unequal,
    }
}
