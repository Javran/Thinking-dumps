/// Check a Luhn checksum.
pub fn is_valid(code: &str) -> bool {
    // iterates from right to left skipping whitespaces.
    let filtered = code.chars().rev().filter(|ch| !ch.is_whitespace());
    // One digit string is invalid.
    if filtered.clone().take(2).count() <= 1 {
        return false;
    }
    let mut sum = 0;
    for (i, ch) in filtered.enumerate()
    {
        if let Some(mut d) = ch.to_digit(10) {
            sum += if i % 2 == 0 {
                d
            } else {
                d *= 2;
                if d > 9 {
                    d -= 9;
                }
                d
            };
        } else {
            return false;
        }
    }

    sum % 10 == 0
}
