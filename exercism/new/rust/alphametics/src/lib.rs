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
struct PartialSolution {
    unassignedDigits: HashSet<u8>,
    partialAssigns: HashMap<char, HashSet<u8>>,
    solvedAssigns: HashMap<char, u8>,
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

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    let puzzle = parse_input(input);
    println!("{:?}", puzzle);
    unimplemented!("Solve the alphametic {:?}", input)
}
