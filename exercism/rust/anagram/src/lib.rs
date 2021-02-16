use std::collections::HashSet;

/// To normalize a word is to first turn it into lowercase and then sort it by char.
/// Both lowered string and sorted vector are returned.
fn normalize(word: &str) -> (String, Vec<char>) {
    let word_lower = word.to_lowercase();
    let mut word_norm: Vec<char> = word_lower.chars().collect();
    word_norm.sort_unstable();
    (word_lower, word_norm)
}

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let (word_lower, word_norm) = normalize(&word);
    possible_anagrams
        .iter()
        .filter(|cur_word| {
            let (cur_word_lower, cur_word_norm) = normalize(cur_word);
            // The goal is to find words that:
            // - has same bag of chars, achieved by sorting by chars.
            // - but not the same sequence of values, achieved by comparing them over lowered strings.
            cur_word_norm == word_norm && cur_word_lower != word_lower
        })
        .copied()
        .collect()
}
