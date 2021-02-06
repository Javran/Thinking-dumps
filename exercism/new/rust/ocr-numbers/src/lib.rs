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
#[derive(PartialEq)]
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
                            .map(|l: &&str| l[i..i + GRID_COLS].to_string())
                            .collect(),
                    )
                })
                .collect()
        })
        .collect())
}

struct GridSample {
    pub grid: Grid,
    pub digit: char,
}

struct GridRecognition(Vec<GridSample>);

impl GridRecognition {
    fn new() -> Self {
        #[rustfmt::skip]
        let raw_samples =
              "    _  _     _  _  _  _  _  _ \n".to_string()
            + "  | _| _||_||_ |_   ||_||_|| |\n"
            + "  ||_  _|  | _||_|  ||_| _||_|\n"
            + "                              ";
        let digits = "1234567890";
        let mut grids2d = split_to_grids(&raw_samples).unwrap();
        assert_eq!(
            grids2d.len(),
            1,
            "raw sample should contain a single digit line."
        );
        assert_eq!(
            grids2d[0].len(),
            digits.len(),
            "sample length matches digit length."
        );
        let grids = grids2d.remove(0);
        GridRecognition(
            grids
                .into_iter()
                .zip(digits.chars())
                .map(|(g, d)| GridSample { grid: g, digit: d })
                .collect(),
        )
    }

    pub fn recognize(&self, grid: &Grid) -> Option<char> {
        self.0
            .iter()
            .filter(|s| &s.grid == grid)
            .map(|s| s.digit)
            .next()
    }
}

pub fn convert(input: &str) -> Result<String, Error> {
    let gr = GridRecognition::new();
    let grids: Vec<Vec<Grid>> = split_to_grids(input)?;
    Ok(grids
        .into_iter()
        .map(|digit_line| {
            digit_line
                .into_iter()
                .map(|g| gr.recognize(&g).unwrap_or('?'))
                .collect::<String>()
        })
        .collect::<Vec<String>>()
        .join(","))
}
