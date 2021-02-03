use std::collections::HashSet;

fn normalize(word: &str) -> Vec<char> {
    let mut word_norm: Vec<char> = word.chars().collect();
    word_norm.sort_unstable();
    return word_norm;
}

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let word_norm: Vec<char> = normalize(word);
    let mut result = HashSet::new();
    for cur_word in possible_anagrams {
        if normalize(cur_word) == word_norm && word != *cur_word {
            result.insert(*cur_word);
        }
    }
    return result;
}
