pub struct PascalsTriangle(u32);

fn next_layer(xs: &[u32]) -> Vec<u32> {
    let mut ys: Vec<u32> = vec![0; xs.len() + 1];
    ys[..xs.len()].copy_from_slice(xs);
    for i in 0..xs.len() {
        ys[i + 1] += xs[i];
    }
    ys
}

impl PascalsTriangle {
    pub fn new(row_count: u32) -> Self {
        PascalsTriangle(row_count)
    }

    pub fn rows(&self) -> Vec<Vec<u32>> {
        (0..self.0)
            .scan(vec![1], |st, _| {
                let ys = st.clone();
                *st = next_layer(st);
                Some(ys)
            })
            .collect()
    }
}
