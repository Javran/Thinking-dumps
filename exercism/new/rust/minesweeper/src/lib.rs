// The design choice here is to either:
// (1) store the minefield as it is with consecutive memory
// (2) or just store mine coordinates and minefield dimensions.
// I'll do (2) as it scales better when minefield becomes larger.

use std::collections::HashMap;
use std::collections::HashSet;

type Coord = (i32, i32);

struct Minefield {
    // Dimension of the minefield, represented as `(rows, cols)`
    dimension: (usize, usize),
    mines: HashSet<Coord>,
}

fn parse(input: &[&str]) -> Minefield {
    let rows = {
        if input.is_empty() {
            return Minefield {
                dimension: (0, 0),
                mines: HashSet::new(),
            };
        }
        input.len()
    };
    let cols = {
        let c = input[0].len();
        assert!(
            input.iter().all(|row| row.len() == c),
            "Row length must be consistent."
        );
        c
    };
    let mines = input
        .iter()
        .enumerate()
        .flat_map(|(r, row): (usize, &&str)| {
            row.char_indices().filter_map(move |(c, ch)| {
                if ch == '*' {
                    Some((r as i32, c as i32))
                } else {
                    None
                }
            })
        })
        .collect::<HashSet<Coord>>();
    Minefield {
        dimension: (rows, cols),
        mines,
    }
}

fn count_mines(mines: &HashSet<Coord>) -> HashMap<Coord, u32> {
    let mut counts = HashMap::new();
    mines.iter().for_each(|(row, col)| {
        (row - 1..=row + 1).for_each(|r| {
            (col - 1..=col + 1).for_each(|c| {
                // Each mine contributes to the 3x3 area around it.
                // note that this is conditionless so mine coordinates itself
                // and out-of-bound coordinates receive counts too.
                // This is fine because struct Minefield stores sufficient info
                // to tell whether a count is spurious.
                counts.entry((r, c)).and_modify(|e| *e += 1).or_insert(1);
            })
        })
    });
    counts
}

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    let Minefield {
        dimension: (rows, cols),
        mines,
    } = parse(minefield);
    let counts = count_mines(&mines);

    (0..rows as i32)
        .map(|r| {
            (0..cols as i32)
                .map(|c| {
                    let coord = (r, c);
                    if mines.contains(&coord) {
                        return '*';
                    }
                    match counts.get(&coord) {
                        None => ' ',
                        Some(x) => char::from(b'0' + *x as u8),
                    }
                })
                .collect::<String>()
        })
        .collect()
}
