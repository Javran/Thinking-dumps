use std::collections::HashMap;
use std::collections::HashSet;
/// Represents a unknown number represented by a sequence of symbols
/// with the least significant digit as first element.
/// so if it's "ABCD" in the original puzzle, it's represented "DCBA" here.
type SymbolNum = Vec<char>;

/// Parsed puzzle representation.
/// All Vec in this structure represents least significant digit as first element.
#[derive(Debug)]
struct Puzzle {
    /// Each element represents a freqency count of symbols for that digit.
    lhs_digits: Vec<HashMap<char, u8>>,
    /// Right Hand Side of the equation.
    rhs: SymbolNum,
    /// Represents symbol assigning order.
    /// lhs_needs[i] is a Vec of chars so that when those chars are all assigned a value,
    /// we can evaluate LHS from least siginificant digit
    /// to the i-th digit counting from the least significant one.
    ///
    /// example:
    ///    HE
    ///  SEES
    /// + THE
    /// -----
    /// LIGHT
    ///
    /// will first produce:
    /// [{E,S}, {H, E}, {E, T}, {S}]
    /// since there is no need of visiting already assigned values, we can remove elements
    /// that has already show up in previous arrays, resulting in:
    /// [{E,S}, {H}, {T}, {}]
    lhs_needs: Vec<Vec<char>>,
    /// a set of chars that should not be assigned 0 in this puzzle.
    non_zeros: HashSet<char>,
}

/// A partial solution to the corresponding puzzle.
#[derive(Debug, Clone)]
struct PartialSolution {
    /// All not-yet-assigned digits in 0..9.
    unassigned_digits: HashSet<u8>,
    /// All already fixed assignments.
    assigns: HashMap<char, u8>,
}

/// Parse input puzzle and preprocess some info for solving algorithm.
fn parse_and_prepare(input: &str) -> Option<(Puzzle, PartialSolution)> {
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
    let mut lhs_digits = vec![HashMap::new(); vlen];
    lhs.iter().for_each(|num: &SymbolNum| {
        num.iter().enumerate().for_each(|(i, ch)| {
            lhs_digits[i]
                .entry(*ch)
                .and_modify(|e| *e += 1)
                .or_insert(1);
        })
    });

    let lhs_needs: Vec<Vec<char>> = {
        let mut needs_acc: HashSet<char> = HashSet::new();
        lhs_digits
            .iter()
            .map(|ds| {
                let s = ds
                    .keys()
                    .filter(|k| !needs_acc.contains(&k))
                    .copied()
                    .collect::<Vec<char>>();
                s.iter().for_each(|ch| {
                    needs_acc.insert(*ch);
                });
                s
            })
            .collect()
    };
    let non_zeros: HashSet<char> = {
        let mut nz = lhs
            .iter()
            .map(|s| *s.last().unwrap())
            .collect::<HashSet<char>>();
        nz.insert(*rhs.last().unwrap());
        nz
    };
    let puzzle = Puzzle {
        lhs_digits,
        rhs,
        lhs_needs,
        non_zeros,
    };

    let solution = {
        PartialSolution {
            unassigned_digits: (0..=9).collect(),
            assigns: HashMap::new(),
        }
    };

    Some((puzzle, solution))
}

fn verify(puzzle: &Puzzle, sol: &PartialSolution, depth: Option<usize>) -> Option<PartialSolution> {
    let mut extra: HashMap<char,u8> = HashMap::new();
    let mut carry: u32 = 0;
    for i in 0..=depth.unwrap_or(puzzle.rhs.len()-1) {
        // compute lhs at position i.
        let mut lhs_sum: u32 = carry;
        if i < puzzle.lhs_digits.len() {
            for (ch, cnt) in &puzzle.lhs_digits[i] {
                let tmp = *sol.assigns.get(&ch)? as u32;
                lhs_sum += tmp * (*cnt as u32);
            }
        }
        let expect_d = (lhs_sum % 10) as u8;
        // match it against rhs.
        if let Some(rval) = sol.assigns.get(&puzzle.rhs[i]) {
            if *rval != expect_d {
                return None
            }
        } else {
            // rhs char not bound yet.
            if !sol.unassigned_digits.contains(&expect_d) {
                return None
            }
            if expect_d == 0 && puzzle.non_zeros.contains(&puzzle.rhs[i]) {
                return None
            }
            extra.insert(puzzle.rhs[i], expect_d);
        }
        carry = lhs_sum / 10;
    }
    if depth.is_none() && carry != 0 {
        return None
    }
    let mut result = sol.clone();
    extra.into_iter().for_each(|(k,v)| {
        result.assigns.insert(k,v);
        result.unassigned_digits.remove(&v);
    });
    Some(result)
}

/// Depth-first searches a digit to assign to `puzzle.lhs_needs[depth][i]`
/// and verifies that the resulting partial solution works.
/// The verification process might produce extra value bindings as a consequence of matching against RHS.
fn search(puzzle: &Puzzle, sol: &mut PartialSolution, depth: usize, i: usize) -> Option<PartialSolution>{
    if depth == puzzle.lhs_needs.len() {
        let sol_v = verify(puzzle, sol, None)?;
        return Some(sol_v)
    }
    let cur_needs = &puzzle.lhs_needs[depth];
    if i >= cur_needs.len() {
        let mut sol_v = verify(puzzle, sol, Some(depth))?;
        return search(puzzle, &mut sol_v, depth + 1, 0);
    }
    let cur_char = cur_needs[i];
    if sol.assigns.contains_key(&cur_char) {
        return search(puzzle, sol, depth, i + 1);
    }
    let non_zero = puzzle.non_zeros.contains(&cur_char);
    let alts: Vec<u8> = 
        sol.unassigned_digits.iter().filter(|i| {!non_zero || **i != 0}).copied().collect();

    for val in alts {
        sol.unassigned_digits.remove(&val);
        sol.assigns.insert(cur_char, val);

        if let Some(r) = search(puzzle, sol, depth, i +1) {
            return Some(r)
        }

        sol.assigns.remove(&cur_char);
        sol.unassigned_digits.insert(val);
    }
    None
}

pub fn solve(input: &str) -> Option<HashMap<char, u8>> {
    let (puzzle, mut sol) = parse_and_prepare(input)?;
    let result = search(&puzzle, &mut sol, 0, 0)?;
    Some(result.assigns)
}
