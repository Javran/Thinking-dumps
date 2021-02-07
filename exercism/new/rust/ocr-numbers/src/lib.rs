const GRID_ROWS: usize = 4;
const GRID_COLS: usize = 3;

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidRowCount(usize),
    InvalidColumnCount(usize),
    // New error category to indicate that
    // the input string does not form a rectangle.
    ColumnCountInconsistent,
}

/// A Grid is a Vec of GRID_ROWS elements,
/// each of them is a string with GRID_COLS characters.
#[derive(PartialEq)]
struct Grid<'a>(Vec<&'a str>);

/// Splits a raw input string into 2D Vec of Grids.
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
        .map(
            // process one "logical line",
            // which are GRID_ROWS lines of raw_lines
            |digits_line: &[&str]| -> Vec<Grid> {
                // separate one logical line into Grids.
                (0..raw_cols)
                    .step_by(GRID_COLS)
                    .map(|i| {
                        Grid(
                            digits_line
                                .iter()
                                .map(|l: &&str| &l[i..i + GRID_COLS])
                                .collect::<Vec<&str>>(),
                        )
                    })
                    .collect()
            },
        )
        .collect())
}

struct GridSample<'a> {
    grid: Grid<'a>,
    digit: char,
}

struct GridRecognition<'a>(Vec<GridSample<'a>>);

impl<'a> GridRecognition<'a> {
    fn new() -> Self {
        // zipping through predefined samples in preparation of doing recognization.
        #[rustfmt::skip]
        let raw_samples = concat!(
            "    _  _     _  _  _  _  _  _ \n",
            "  | _| _||_||_ |_   ||_||_|| |\n",
            "  ||_  _|  | _||_|  ||_| _||_|\n",
            "                              ");
        let digits = "1234567890";
        let grids = {
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
            grids2d.remove(0)
        };

        GridRecognition(
            grids
                .into_iter()
                .zip(digits.chars())
                .map(|(grid, digit)| GridSample { grid, digit })
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
