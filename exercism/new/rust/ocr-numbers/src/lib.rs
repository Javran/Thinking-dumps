use std::slice::Chunks;

const GRID_ROWS: usize = 4;
const GRID_COLS: usize = 3;

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidRowCount(usize),
    InvalidColumnCount(usize),
    ColumnCountInconsistent,
}

// A Grid is a Vec of 4 elements,
// each of them is a string with 3 characters.
struct Grid(Vec<String>);

fn split_to_grids(input: &str) -> Result<Vec<Vec<Grid>>, Error> {
    let raw_lines: Vec<&str> = input.lines().collect();
    let raw_rows = raw_lines.len();
    if raw_rows == 0 {
        return Ok(vec![]);
    }
    if raw_rows % GRID_ROWS != 0 {
        return Err(Error::InvalidRowCount(raw_rows));
    }
    let raw_cols = raw_lines[0].len();
    if raw_cols % GRID_COLS != 0 {
        return Err(Error::InvalidColumnCount(raw_cols));
    }
    if raw_lines.iter().any(|l| l.len() != raw_cols) {
        return Err(Error::ColumnCountInconsistent);
    }

    Ok(raw_lines
        .chunks(GRID_ROWS)
        .into_iter()
        .map(|digits_line: &[&str]| -> Vec<Grid> {
            // turn digits_line into a Vec of Grids
            (0..raw_cols)
                .step_by(GRID_COLS)
                .map(|i| {
                    Grid(
                        digits_line
                            .into_iter()
                            .map(|l: &&str| -> String {
                                l[i.. i + GRID_COLS].to_string()
                            })
                            .collect()
                    )
                })
                .collect()
        })
        .collect())
}

pub fn convert(input: &str) -> Result<String, Error> {
    let v = split_to_grids(input)?;
    unimplemented!("Convert the input '{}' to a string", input);
}
