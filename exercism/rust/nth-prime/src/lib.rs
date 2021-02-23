
fn is_odd_prime(n: u32) -> bool {
    if n <= 1 {
        return false
    }
    let up_bound = (n as f64).sqrt().ceil() as u32;
    for i in 3 ..= up_bound {
        if n % i == 0 {
            return false
        }
    }
    true
}

pub fn nth(n: u32) -> u32 {
    if n == 0 {
        return 2
    }
    let mut count = 0;
    for i in (3..).step_by(2) {
        if is_odd_prime(i) {
            count += 1;
        }
        if count == n {
            return i;
        }
    }
    unreachable!()
}
