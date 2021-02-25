use std::collections::HashMap;
use std::collections::VecDeque;

#[derive(PartialEq, Eq, Debug)]
pub enum Bucket {
    One,
    Two,
}

/// A struct to hold your results in.
#[derive(PartialEq, Eq, Debug)]
pub struct BucketStats {
    /// The total number of "moves" it should take to reach the desired number of liters, including
    /// the first fill.
    pub moves: u8,
    /// Which bucket should end up with the desired number of liters? (Either "one" or "two")
    pub goal_bucket: Bucket,
    /// How many liters are left in the other bucket?
    pub other_bucket: u8,
}

enum Move {
    /// `Pour(a)` pours from bucket `a` into the other one.
    Pour(Bucket),
    /// Empties the bucket.
    Empty(Bucket),
    /// Fills the bucket.
    Fill(Bucket),
}

type State = (u8, u8);

/// Solve the bucket problem
pub fn solve(
    capacity_1: u8,
    capacity_2: u8,
    goal: u8,
    start_bucket: &Bucket,
) -> Option<BucketStats> {
    let next_moves = |s: State| -> Vec<(State, Move)> {
        let mut moves = vec![];
        let (b_1, b_2) = s;
        let headroom_1 = capacity_1 - b_1;
        let headroom_2 = capacity_2 - b_2;

        // Actions on Bucket one:
        if b_1 > 0 && headroom_2 > 0 {
            if b_1 < headroom_2 {
                moves.push(((0, b_2 + b_1), Move::Pour(Bucket::One)));
            } else {
                moves.push(((b_1 - headroom_2, capacity_2), Move::Pour(Bucket::One)));
            }
        }
        if b_1 > 0 {
            moves.push(((0, b_2), Move::Empty(Bucket::One)));
        }
        if headroom_1 > 0 {
            moves.push(((capacity_1, b_2), Move::Fill(Bucket::One)));
        }

        // Actions on Bucket two:
        if b_2 > 0 && headroom_1 > 0 {
            if b_2 < headroom_1 {
                moves.push(((b_1 + b_2, 0), Move::Pour(Bucket::Two)));
            } else {
                moves.push(((capacity_1, b_2 - headroom_1), Move::Pour(Bucket::Two)));
            }
        }
        if b_2 > 0 {
            moves.push(((b_1, 0), Move::Empty(Bucket::Two)));
        }
        if headroom_2 > 0 {
            moves.push(((b_1, capacity_2), Move::Fill(Bucket::Two)));
        }

        moves
    };

    // standard breadth first search
    let (init_state, forbidden_state) = match start_bucket {
        Bucket::One => ((capacity_1, 0), (0, capacity_2)),
        Bucket::Two => ((0, capacity_2), (capacity_1, 0)),
    };
    let mut discovered: HashMap<State, u8> = HashMap::new();
    discovered.insert(init_state, 0);

    let mut q: VecDeque<(State, u8)> = vec![
        // I have no idea why moves starts at 1 even
        // in cases where the start state is already the final state.
        // but the test suite demands so.
        (init_state, 1),
    ]
    .into_iter()
    .collect();

    while let Some((st, depth)) = q.pop_front() {
        let (b_1, b_2) = st;
        if b_1 == goal {
            return Some(BucketStats {
                moves: depth,
                goal_bucket: Bucket::One,
                other_bucket: b_2,
            });
        }
        if b_2 == goal {
            return Some(BucketStats {
                moves: depth,
                goal_bucket: Bucket::Two,
                other_bucket: b_1,
            });
        }

        // the Move value is ignored but we can attach that to `discovered`
        // if we want to print out the exact sequence of moves.
        for (s_next, _move) in next_moves(st) {
            if s_next != forbidden_state && !discovered.contains_key(&s_next) {
                discovered.insert(s_next, depth + 1);
                q.push_back((s_next, depth + 1));
            }
        }
    }

    None
}
