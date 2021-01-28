use divrem::DivRem;
use std::fmt;

const MINS_PER_DAY: i32 = 24 * 60;

#[derive(PartialEq, Debug)]
pub struct Clock {
    // INVARIANT: 0 <= minute && minute < MINS_PER_DAY
    minute: i32,
}

/// Performs division and wraps the result `r` so that `0 <= r && r < divisor`.
///
/// It is required that both `divisor` and `-divisor` are representable in `i32`,
/// but this condition is unchecked.
fn div_nonneg(val: i32, divisor: i32) -> i32 {
    let result: i32 = val % divisor;
    if result < 0 {
        divisor + result
    } else {
        result
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let hh = div_nonneg(hours, 24);
        let mm = div_nonneg(minutes, MINS_PER_DAY);
        // INVARIANT: hh >= 0 && hh < 24 && mm >= 0 && mm < MINS_PER_DAY
        Clock {
            minute: (hh * 60 + mm) % MINS_PER_DAY,
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock::new(0, self.minute + div_nonneg(minutes, MINS_PER_DAY))
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (hh, mm) = self.minute.div_rem(60);
        write!(f, "{:02}:{:02}", hh, mm)
    }
}
