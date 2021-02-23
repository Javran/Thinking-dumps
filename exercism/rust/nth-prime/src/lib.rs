fn is_odd_prime(n: u32) -> bool {
    if n <= 1 {
        return false;
    }
    let upbound = (n as f64).sqrt().ceil() as u32;
    (3..=upbound).all(|i| n % i != 0)
}

// The straightforward approach: prime-test on odd numbers and keep track of the count.
pub fn nth(n: u32) -> u32 {
    std::iter::once(2)
        .chain((3..).step_by(2).filter(|i| is_odd_prime(*i)))
        .nth(n as usize)
        .unwrap()
}

// An alternative idea by sieving but this overflows the stack on large input:
// - Start with it = `2..`,
// - take out next prime `p` by `it.next()`
// - replace `it` with `it.filter(|u| u % p != 0)`, so all multiples of `p` are removed from `it`
// - repeat the process.

struct Sieve<'a> {
    src: Box<dyn Iterator<Item = u32> + 'a>,
}

impl<'a> Iterator for Sieve<'a> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        let nx: Option<u32> = self.src.next();
        let cur: u32 = nx.unwrap();
        let src = std::mem::replace(&mut self.src, Box::new(std::iter::empty()));
        self.src = Box::new(src.filter(move |u| u % cur != 0));
        nx
    }
}

pub fn nth_sieve_method(n: u32) -> u32 {
    Sieve { src: Box::new(2..) }.nth(n as usize).unwrap()
}
