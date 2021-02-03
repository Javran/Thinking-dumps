use std::collections::HashSet;

fn normalize(word: &str) -> Vec<char> {
    let mut word_norm: Vec<char> = word.chars().collect();
    word_norm.sort_unstable();
    word_norm
}

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let word_lower = word.to_lowercase();
    let word_norm: Vec<char> = normalize(&word_lower);
    let mut result = HashSet::new();
    for cur_word in possible_anagrams {
        let cur_word_lower = cur_word.to_lowercase();
        if normalize(&cur_word_lower) == word_norm && cur_word_lower != word_lower {
            result.insert(*cur_word);
        }
    }
    result
}
