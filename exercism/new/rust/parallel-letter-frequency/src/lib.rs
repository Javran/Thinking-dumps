use std::thread;
use std::collections::HashMap;
use std::sync::Arc;

pub fn frequency_single_worker(input: &[String]) -> HashMap<char, usize> {
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

pub fn frequency(input: &[&str], mut worker_count: usize) -> HashMap<char, usize> {
    if input.len() < worker_count {
        worker_count = input.len();
    }

    let chunk_sz: usize = (input.len() as f64 / worker_count as f64).ceil() as usize;
    let input2: Vec<String> = input.iter().map(|s| s.to_string()).collect();
    let l = input2.len();
    let shared = Arc::new(input2);

    let mut threads = vec![];
    for i in (0..l).step_by(chunk_sz) {
        let cur = Arc::clone(&shared);
        // let tx_cur = tx.clone();
        threads.push(
            thread::spawn(move || {
                let result: HashMap<char, usize> = frequency_single_worker(&cur[i..(i+chunk_sz).min(l)]);
                result
            }));
    }

    let mut result = HashMap::new();
    for t in threads.into_iter() {
        let r = t.join().unwrap();
        for (ch, count) in r.into_iter() {
            result
                .entry(ch)
                .and_modify(|e| *e += count)
                .or_insert(count);
        }
    }

    result
}
