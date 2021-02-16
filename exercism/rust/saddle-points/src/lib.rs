type Coord = (usize, usize);

pub fn find_saddle_points(input: &[Vec<u64>]) -> Vec<Coord> {
    let rows = input.len();
    if rows == 0 {
        return vec![];
    }
    let cols = input[0].len();
    assert!(
        input.iter().all(|row| row.len() == cols),
        "Row length must be consistent"
    );
    if cols == 0 {
        return vec![];
    }
    // The saddle point can only come from:
    // - max value points of each row
    // - min value points of each col
    // So we just need to find those two sets of values their intersection is the answer.
    let row_max_vals: Vec<u64> = input
        .iter()
        .map(|row| row[1..].iter().fold(row[0], |acc, x| acc.max(*x)))
        .collect();
    let col_min_vals: Vec<u64> = (0..cols).map(|c| {
        let col: Vec<u64> = input.iter().map(|row| row[c]).collect();
        col[1..].iter().fold(col[0], |acc, x| acc.min(*x))
    }).collect();
    
    input.iter().enumerate().flat_map( |(r, row): (usize, &Vec<u64>)| {
        row.iter().enumerate().filter_map( |(c, val): (usize, &u64)| {
            if row_max_vals[r] == *val && col_min_vals[c] == *val {
                Some((r,c))
            } else {
                None
            }
        }).collect::<Vec<Coord>>()
    }).collect()
}
