type Coord = (i32, i32);

#[derive(Debug)]
pub struct ChessPosition(Coord);

#[derive(Debug)]
pub struct Queen(Coord);

impl ChessPosition {
    pub fn new(rank: i32, file: i32) -> Option<Self> {
        if rank >= 0 && rank <= 7 && file >= 0 && file <= 7 {
            Some(ChessPosition((rank, file)))
        } else {
            None
        }
    }
}

impl Queen {
    pub fn new(position: ChessPosition) -> Self {
        Self(position.0)
    }

    pub fn can_attack(&self, other: &Queen) -> bool {
        let (r0, f0): Coord = self.0;
        let (r1, f1): Coord = other.0;

        r0 == r1 || f0 == f1 || r0 + f0 == r1 + f1 || r0 - f0 == r1 - f1
    }
}
