use divrem::DivRem;
use std::fmt;

const MINS_PER_DAY: i32 = 24 * 60;

// INVARIANT: 0 <= minute && minute < MINS_PER_DAY
#[derive(PartialEq, Debug)]
pub struct Clock(i32);

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let hh = hours.rem_euclid(24);
        let mm = minutes.rem_euclid(MINS_PER_DAY);
        // INVARIANT: hh >= 0 && hh < 24 && mm >= 0 && mm < MINS_PER_DAY
        Clock((hh * 60 + mm) % MINS_PER_DAY)
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Clock::new(0, self.0 + minutes.rem_euclid(MINS_PER_DAY))
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (hh, mm) = self.0.div_rem(60);
        write!(f, "{:02}:{:02}", hh, mm)
    }
}
