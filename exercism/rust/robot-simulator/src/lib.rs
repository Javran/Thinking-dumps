use num_enum::TryFromPrimitive;
use std::convert::TryFrom;

#[derive(PartialEq, Debug, TryFromPrimitive, Clone, Copy)]
#[repr(u8)]
pub enum Direction {
    North = 0,
    East = 1,
    South = 2,
    West = 3,
}

type Coord = (i32, i32);

impl Direction {
    pub fn left(&self) -> Self {
        Direction::try_from((*self as u8 + 3) & 0b11).unwrap()
    }
    pub fn right(&self) -> Self {
        Direction::try_from((*self as u8 + 1) & 0b11).unwrap()
    }
    pub fn advance(&self, (x, y): Coord) -> Coord {
        use Direction::*;
        match self {
            North => (x, y + 1),
            South => (x, y - 1),
            East => (x + 1, y),
            West => (x - 1, y),
        }
    }
}

pub struct Robot {
    coord: Coord,
    dir: Direction,
}

impl Robot {
    pub fn new(x: i32, y: i32, dir: Direction) -> Self {
        Robot { coord: (x, y), dir }
    }

    pub fn turn_right(self) -> Self {
        Robot {
            coord: self.coord,
            dir: self.dir.right(),
        }
    }

    pub fn turn_left(self) -> Self {
        Robot {
            coord: self.coord,
            dir: self.dir.left(),
        }
    }

    pub fn advance(self) -> Self {
        Robot {
            coord: self.dir.advance(self.coord),
            dir: self.dir,
        }
    }

    pub fn instructions(self, instructions: &str) -> Self {
        instructions.chars().fold(self, |acc, instr| match instr {
            'R' => acc.turn_right(),
            'L' => acc.turn_left(),
            'A' => acc.advance(),
            _ => panic!("unknown instruction."),
        })
    }

    pub fn position(&self) -> (i32, i32) {
        self.coord
    }

    pub fn direction(&self) -> &Direction {
        &self.dir
    }
}
