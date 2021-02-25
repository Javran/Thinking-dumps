pub fn brackets_are_balanced(s: &str) -> bool {
    // keeps track of a stack of expected matching right parentheses.
    let mut expect_r: Vec<char> = vec![];
    for ch in s.chars() {
        match ch {
            '(' => expect_r.push(')'),
            '[' => expect_r.push(']'),
            '{' => expect_r.push('}'),
            ')' | ']' | '}' => match expect_r.pop() {
                Some(e) if e == ch => {}
                _ => return false,
            },
            _ => {}
        }
    }
    expect_r.is_empty()
}
