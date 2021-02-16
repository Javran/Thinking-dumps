struct Minefield<'a> {
    // Dimension of the minefield, represented as `(rows, cols)`
    dimension: (usize, usize),
    field: Vec<&'a [u8]>,
}

fn parse<'a>(input: &'a [&'a str]) -> Minefield<'a> {
    let rows = {
        if input.is_empty() {
            return Minefield {
                dimension: (0, 0),
                field: vec![],
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
    let mines = input.iter().map(|s: &&str| s.as_bytes()).collect();
    Minefield {
        dimension: (rows, cols),
        field: mines,
    }
}

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    let Minefield {
        dimension: (rows, cols),
        field,
    } = parse(minefield);

    (0..rows as i32)
        .map(|row| {
            (0..cols as i32)
                .map(|col| {
                    if field[row as usize][col as usize] == b'*' {
                        return '*';
                    }
                    let mut count = 0;
                    for r in row - 1..=row + 1 {
                        if let Some(row_vec) = field.get(r as usize) {
                            for c in col - 1..=col + 1 {
                                if let Some(ch) = row_vec.get(c as usize) {
                                    if *ch == b'*' {
                                        count += 1;
                                    }
                                }
                            }
                        }
                    }
                    if count == 0 {
                        ' '
                    } else {
                        std::char::from_digit(count, 10).unwrap()
                    }
                })
                .collect::<String>()
        })
        .collect()
}
