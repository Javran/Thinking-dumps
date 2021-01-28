use std::fmt;

#[derive(PartialEq, Debug)]
pub struct Clock {
    minute: u32,
}

fn normalize(val: i32, divisor: u32) -> u32 {
    let result = val % (divisor as i32);
    if result < 0 {
        ((divisor as i32) + result) as u32
    } else {
        result as u32
    }
}

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let hh = normalize(hours, 24);
        let mm = normalize(minutes, 24 * 60);
        Clock {
            minute: (hh * 60 + mm) % (24 * 60),
        }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        let mm_add = normalize(minutes, 24 * 60);
        Clock {
            minute: (self.minute + mm_add) % (24 * 60),
        }
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mm = self.minute % 60;
        let hh = self.minute / 60;
        write!(f, "{:02}:{:02}", hh, mm)
    }
}
