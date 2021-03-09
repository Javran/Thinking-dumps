pub struct Triangle((u64, u64, u64));

impl Triangle {
    pub fn build(sides: [u64; 3]) -> Option<Triangle> {
        use std::mem::swap;
        let [mut a, mut b, mut c] = sides;
        // Apply sorting network on 3 elements.
        if a > b {
            swap(&mut a, &mut b);
        }
        if b > c {
            swap(&mut b, &mut c);
        }
        if a > b {
            swap(&mut a, &mut b);
        }

        // INVARIANT: a <= b <= c.
        // b + c > a and a + c > b are obvious, leaving only one to verify:
        if a + b > c && a > 0 {
            Some(Triangle((a, b, c)))
        } else {
            None
        }
    }

    pub fn is_equilateral(&self) -> bool {
        self.0 .0 == self.0 .2
    }

    pub fn is_scalene(&self) -> bool {
        !self.is_isosceles()
    }

    pub fn is_isosceles(&self) -> bool {
        self.0 .0 == self.0 .1 || self.0 .1 == self.0 .2
    }
}
