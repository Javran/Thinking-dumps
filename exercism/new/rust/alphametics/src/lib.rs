use std::collections::HashMap;
use std::collections::HashSet;
/// Represents a unknown number represented by a sequence of symbols
/// with least significant digit first.
/// so if it's "ABCD" in the original puzzle, it's represented "DCBA" here.
type SymbolNum = Vec<char>;

/// Parsed puzzle representation.
#[derive(Debug)]
struct Puzzle {
    lhs: Vec<SymbolNum>,
    lhs_accumulated: Vec<HashMap<char, u8>>,
    rhs: Vec<char>,
    symbol_sets: Vec<HashSet<char>>,
    ordered_symbols: Vec<char>,
}

/// A partial solution to the corresponding puzzle.
#[derive(Debug)]
struct PartialSolution {
    unassigned_digits: HashSet<u8>,
    partial_assigns: HashMap<char, HashSet<u8>>,
    solved_assigns: HashMap<char, u8>,
}

/// Parse input puzzle and preprocess some info for solving algorithm.
fn parse_and_prepare(input: &str) -> Option<Puzzle> {
    let sides: Vec<&str> = input.split(" == ").collect();
    if sides.len() != 2 {
        return None;
    }
    let to_sym_num = |xs: &str| xs.chars().rev().collect();
    let lhs: Vec<SymbolNum> = sides[0].split(" + ").map(to_sym_num).collect();
    if lhs.is_empty() {
        return None;
    }
    let vlen = lhs.iter().fold(0, |acc, x| acc.max(x.len()));
    let rhs = to_sym_num(sides[1]);
    if vlen > rhs.len() {
        return None;
    }
    let mut lhs_accumulated = vec![HashMap::new(); rhs.len()];
    lhs.iter().for_each(|num: &SymbolNum| {
        num.iter().enumerate().for_each(|(i, ch)| {
            lhs_accumulated[i]
                .entry(*ch)
                .and_modify(|e| *e += 1)
                .or_insert(1);
        })
    });

    let mut symbol_sets: Vec<HashSet<char>> = rhs
        .iter()
        .map(|ch| {
            let mut s = HashSet::new();
            s.insert(*ch);
            s
        })
        .collect();
    lhs.iter().for_each(|num| {
        num.iter().enumerate().for_each(|(i, ch)| {
            symbol_sets[i].insert(*ch);
        })
    });
    let ordered_symbols = {
        let mut seen: HashSet<char> = HashSet::new();
        let mut vs: Vec<char> = Vec::new();
        symbol_sets.iter().for_each(|set| {
            set.iter().for_each(|ch| {
                if !seen.contains(ch) {
                    vs.push(*ch);
                    seen.insert(*ch);
                }
            })
        });
        vs
    };
    Some(Puzzle {
        lhs,
        lhs_accumulated,
        rhs,
        symbol_sets,
        ordered_symbols,
    })
}

fn init_solution(puzzle: &Puzzle) -> PartialSolution {
    let mut non_zeros: HashMap<char, bool> = HashMap::new();
    let mut process_sym = |n: &SymbolNum| {
        for (i, ch) in n.iter().enumerate() {
            let e = non_zeros.entry(*ch).or_insert(false);
            if !*e {
                *e = n.len() - 1 == i;
            }
        }
    };

    for n in puzzle.lhs.iter() {
        process_sym(&n);
    }
    process_sym(&puzzle.rhs);
    PartialSolution {
        unassigned_digits: (0..=9).collect(),
        partial_assigns: non_zeros
            .into_iter()
            .map(|(k, non_zero)| {
                (
                    k,
                    if non_zero {
                        (1..=9).collect()
                    } else {
                        (0..=9).collect()
                    },
                )
            })
            .collect(),
        solved_assigns: HashMap::new(),
    }
}

fn verify(puzzle: &Puzzle, sol: &PartialSolution) -> bool {
    let mut carry: u64 = 0;
    // from least significant to most.
    for i in 0..puzzle.rhs.len() {
        let symbols = &puzzle.symbol_sets[i];
        if symbols.iter().all(|s| sol.solved_assigns.contains_key(s)) {
            // we have all necessary symbols assigned for this position.
            let lhs_cur = carry
                + puzzle.lhs_accumulated[i]
                    .iter()
                    .map(|(ch, count)| {
                        (*sol.solved_assigns.get(&ch).unwrap() as u64) * (*count as u64)
                    })
                    .sum::<u64>();
            let rhs_cur = sol.solved_assigns.get(&puzzle.rhs[i]).unwrap();
            if lhs_cur % 10 != (*rhs_cur as u64) {
                return false;
            }
            carry = lhs_cur / 10;
        } else {
            return true;
        }
    }
    carry == 0
}

fn search(puzzle: &Puzzle, sol: &mut PartialSolution, depth: usize) -> bool {
    if depth == puzzle.ordered_symbols.len() {
        return true;
    }
    // choose a value for next symbol.
    let ch: char = puzzle.ordered_symbols[depth];
    // let mut partial_assigns = &mut sol.partial_assigns;
    let alts: HashSet<u8> = sol.partial_assigns.remove(&ch).unwrap();

    for v in alts.iter() {
        if sol.unassigned_digits.contains(&v) {
            // try assigning ch => v.
            sol.unassigned_digits.remove(&v);
            sol.solved_assigns.insert(ch, *v);

            if verify(puzzle, sol) && search(puzzle, sol, depth + 1) {
                return true;
            }

            // recover
            sol.solved_assigns.remove(&ch);
            sol.unassigned_digits.insert(*v);
        }
    }
    sol.partial_assigns.insert(ch, alts);
    false
}

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    let puzzle = parse_and_prepare(input)?;
    let mut sol = init_solution(&puzzle);
    if search(&puzzle, &mut sol, 0) {
        Some(sol.solved_assigns)
    } else {
        None
    }
}
