use std::collections::HashMap;
use std::sync::mpsc;
use std::sync::Arc;
use std::thread;

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
    if worker_count == 0 {
        panic!("Expected non-zero number of workers.");
    }
    let len = input.len();
    if len == 0 {
        return HashMap::new();
    }
    if len < worker_count {
        // make sure every worker has non-zero amount of tasks to do.
        worker_count = len;
    }

    let len_per_worker: usize = (len as f64 / worker_count as f64).ceil() as usize;
    let shared: Arc<Vec<String>> = Arc::new(input.iter().map(|s| s.to_string()).collect());
    let (tx, rx) = mpsc::channel();
    let mut todo = 0;
    for i in (0..shared.len()).step_by(len_per_worker) {
        let cur = Arc::clone(&shared);
        let tx_cur = tx.clone();
        todo += 1;
        thread::spawn(move || {
            let result: HashMap<char, usize> =
                frequency_single_worker(&cur[i..(i + len_per_worker).min(len)]);
            tx_cur.send(result).unwrap();
        });
    }

    let mut result = HashMap::new();
    for r in rx {
        for (ch, count) in r.into_iter() {
            result
                .entry(ch)
                .and_modify(|e| *e += count)
                .or_insert(count);
        }
        todo -= 1;
        if todo == 0 {
            break;
        }
    }

    result
}
