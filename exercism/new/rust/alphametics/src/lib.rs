use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
/// Represents a unknown number represented by a sequence of symbols
/// with least significant digit first.
/// so if it's "ABCD" in the original puzzle, it's represented "DCBA" here.
type SymbolNum = String;

/// Parsed puzzle representation.
#[derive(Debug)]
struct Puzzle {
    lhs: Vec<SymbolNum>,
    rhs: SymbolNum,
}

/// A partial solution to the corresponding puzzle.
#[derive(Debug)]
struct PartialSolution {
    unassigned_digits: HashSet<u8>,
    partial_assigns: HashMap<char, HashSet<u8>>,
    solved_assigns: HashMap<char, u8>,
}

fn parse_input(input: &str) -> Puzzle {
    let sides: Vec<&str> = input.split(" == ").collect();
    assert_eq!(sides.len(), 2, "Unexpected LHS or RHS");
    let to_sym_num = |xs: &str| xs.chars().rev().collect();
    Puzzle {
        lhs: sides[0].split(" + ").map(to_sym_num).collect(),
        rhs: to_sym_num(sides[1]),
    }
}

fn init_solution(puzzle: &Puzzle) -> PartialSolution {
    let mut non_zeros: HashMap<char, bool> = HashMap::new();
    let mut process_sym = |n: &SymbolNum| {
        for (i, ch) in n.char_indices() {
            match non_zeros.entry(ch) {
                Entry::Occupied(mut oe) => {
                    if !*oe.get() {
                        oe.insert(i == n.len() - 1);
                    }
                }
                Entry::Vacant(ve) => {
                    ve.insert(i == n.len() - 1);
                }
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
    if !sol.partial_assigns.is_empty() {
        return true;
    }

    let to_num = |n: &SymbolNum| -> u64 {
        n.chars().rev().fold(0, |acc, ch| {
            acc * 10 + (*sol.solved_assigns.get(&ch).unwrap() as u64)
        })
    };

    puzzle.lhs.iter().map(to_num).sum::<u64>() == to_num(&puzzle.rhs)
}

fn search(puzzle: &Puzzle, sol: &mut PartialSolution) -> bool {
    if sol.partial_assigns.is_empty() {
        return true;
    }
    // pick a random unassigned char and try to assign a value to it.
    let ch: char = *sol.partial_assigns.iter().next().unwrap().0;
    // let mut partial_assigns = &mut sol.partial_assigns;
    let alts: HashSet<u8> = sol.partial_assigns.remove(&ch).unwrap();

    for v in alts.iter() {
        if sol.unassigned_digits.contains(&v) {
            // try assigning ch => v.
            sol.unassigned_digits.remove(&v);
            sol.solved_assigns.insert(ch, *v);

            if verify(puzzle, sol) && search(puzzle, sol) {
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
    let puzzle = parse_input(input);
    let mut sol = init_solution(&puzzle);
    if search(&puzzle, &mut sol) {
        Some(sol.solved_assigns)
    } else {
        None
    }
}
