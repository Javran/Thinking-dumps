pub fn reverse(input: &str) -> String {
    let mut s = String::with_capacity(input.len());
    for (_, ch) in input.char_indices().rev() {
        s.push(ch);
    }
    s
}
