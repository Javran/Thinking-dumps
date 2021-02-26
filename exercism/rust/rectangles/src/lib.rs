type Coord = (usize, usize);

pub fn count(lines: &[&str]) -> u32 {
    let rows = lines.len();
    if rows == 0 {
        return 0;
    }
    let cols = lines[0].len();
    if lines.iter().any(|line| line.len() != cols) {
        panic!("inconsistent line width")
    };
    let mat: Vec<&[u8]> = lines.iter().map(|x| x.as_bytes()).collect();
    let cross_sorted = {
        let mut v: Vec<Coord> = vec![];
        for (i, line) in mat.iter().enumerate() {
            for (j, ch) in line.iter().enumerate() {
                if *ch == b'+' {
                    v.push((i, j));
                }
            }
        }
        v
    };

    let mut cnt = 0;
    // for a given top-left corner coord_0, we want to find a bottom-right corner coord_1.
    for (i, coord_0) in cross_sorted.iter().enumerate() {
        let (x_0, y_0): Coord = *coord_0;
        // since this Vec is sorted, we only need find coord_1 in to all coords following coord_0.
        for coord_1 in cross_sorted.iter().skip(i+1) {
            let (x_1, y_1): Coord = *coord_1;
            if x_0 < x_1 && y_0 < y_1 && mat[x_0][y_1] == b'+' && mat[x_1][y_0] == b'+' {
                let expected_horiz_lines = ((y_0 + 1)..y_1).all(|y| {
                    (mat[x_0][y] == b'-' || mat[x_0][y] == b'+')
                        && (mat[x_1][y] == b'-' || mat[x_1][y] == b'+')
                });
                if !expected_horiz_lines {
                    continue;
                }
                let expected_verti_lines = ((x_0 + 1)..x_1).all(|x| {
                    (mat[x][y_0] == b'|' || mat[x][y_0] == b'+')
                        && (mat[x][y_1] == b'|' || mat[x][y_1] == b'+')
                });
                if !expected_verti_lines {
                    continue;
                }
                cnt += 1;
            }
        }
    }
    cnt
}
