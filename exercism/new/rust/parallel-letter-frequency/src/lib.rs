use std::collections::HashMap;
// use std::sync::mpsc;
// use std::thread;

pub fn frequency_single_worker(input: &[&str]) -> HashMap<char, usize> {
    let mut freq = HashMap::new();
    for line in input {
        for ch in line.to_lowercase().chars() {
            if ch.is_alphabetic() {
                freq.entry(ch).and_modify(|e| *e += 1).or_insert(1);
            }
        }
    }
    freq
}

pub fn frequency(input: &[&str], _worker_count: usize) -> HashMap<char, usize> {
    /*
    let mut result = HashMap::new();

    for r in rx {
        for (ch, count) in r.into_iter() {
            result
                .entry(ch)
                .and_modify(|e| *e += count)
                .or_insert(count);
        }
    }
    result */
    frequency_single_worker(input)
}
