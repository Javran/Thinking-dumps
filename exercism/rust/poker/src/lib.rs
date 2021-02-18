/*
 Spec on card syntax:

 - rank: 2 - 10 or J Q K A
 - suit: D(iamond) H(eart) S(pade) C(lub)
 - no Joker

*/

/*
  Ranking reference: https://en.wikipedia.org/wiki/List_of_poker_hands as of Feb 17, 2021.
*/

/*
 Only stores 1 ~ 13, where 1 => A, 11 => J, 12 => Q, 13 => K.
*/
struct Rank(u8);

type Suit = u8;

struct Card {
    rank: Rank,
    suit: Suit,
}

impl Card {
    pub fn parse(raw: &[u8]) -> Option<Self> {
        match raw.len() {
            2 => {
                let suit = raw[1];
                match raw[0] {
                    b'A' => Some(Card {
                        rank: Rank(1),
                        suit,
                    }),
                    b'J' => Some(Card {
                        rank: Rank(11),
                        suit,
                    }),
                    b'Q' => Some(Card {
                        rank: Rank(12),
                        suit,
                    }),
                    b'K' => Some(Card {
                        rank: Rank(13),
                        suit,
                    }),
                    b'0'..=b'9' => Some(Card {
                        rank: Rank(raw[0] - b'0'),
                        suit,
                    }),
                    _ => None,
                }
            }
            3 if &raw[..2] == "10".as_bytes() => Some(Card {
                rank: Rank(10),
                suit: raw[2],
            }),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct Hand<'a> {
    src: &'a str,
    rank: HandRank,
}

impl<'a> Hand<'a> {
    pub fn parse(src: &'a str) -> Option<Self> {
        let mut cards: Vec<Card> = Vec::with_capacity(5);

        for s in src.split(' ') {
            cards.push(Card::parse(s.as_bytes())?);
        }

        // parsing a hand should always result in exactly 5 cards.
        if cards.len() != 5 {
            return None;
        }

        Some(Hand {
            rank: HandRank::rank(cards),
            src,
        })
    }
}

/// `HandRank` establishes a total order to support sorting.
/*
 This is achieved by:

 - Alternatives are arranged from lowest to highest rank,
   this ensures that higher rank always beats lower rank regardless of numeric value.

 - Suit are dropped from this representation as it's exact value is irrelevant to
   this ranking method.

 - Within the same alternative construct, each value represents a card rank,
   ordered from largest count to lowest count.
   If card count is the same, larger numeric value goes first.

   For example:
   - FourOfAKind(3,2) represents 3,3,3,3,2,2
   - TwoPair(14,13,12) represents A,A,K,K,Q
   - StraightFlush(14) represents A,K,Q,J,10 (a.k.a. Royal flush)

 - whenever `Vec<u8>` appears, it's always of size 5
   (while it makes more sense to use `[u8;5]`, `Vec`'s interface is a bit more convenient).

*/
#[derive(Eq, PartialOrd, Ord, PartialEq, Debug)]
enum HandRank {
    HighCard(Vec<u8>),
    OnePair(u8, u8, u8, u8),
    TwoPair(u8, u8, u8),
    ThreeOfAKind(u8, u8, u8),
    Straight(u8),
    Flush(Vec<u8>),
    FullHouse(u8, u8),
    FourOfAKind(u8, u8),
    StraightFlush(u8),
}

/*
  Used to represent frequency of a rank in a hand, only index 1 ~ 13 are used,
  r[i] means the number of cards with rank i, where r: RankFreq.

  The sum of r must be 5.
*/
type RankFreq = [usize; 14];

/*
 Intermediate structure that store same info as RankFreq,
 but is more convenient for Hand ranking
 elements are (<rank>, <count>), sorted so that:

 - elements with larger <count> appears first.
 - in case <count> is the same, element with higest <rank> goes first.

 Note that unlike `Rank` type, 14 might appear as <rank> representing Ace.
*/
type RankCount = Vec<(u8, usize)>;

fn to_rank_count(r: &RankFreq) -> RankCount {
    let mut xs: RankCount = vec![];
    /*
     Treat Ace as 14, this won't have correctness implication
     as long as the resulting structure is never used to check for Straight.

     The observation is: the only time Ace is treated as 1 is when
     the hand needs it to complete a Straight.
    */
    if r[1] > 0 {
        xs.push((14, r[1]));
    }
    #[allow(clippy::needless_range_loop)]
    for i in 2..=13 {
        if r[i] > 0 {
            xs.push((i as u8, r[i]));
        }
    }
    xs.sort_unstable_by(|x, y| {
        // larger freq first then larger rank
        y.1.cmp(&x.1).then(y.0.cmp(&x.0))
    });

    xs
}

/* Unpacks RankCount to sorted 5 elements */
#[allow(clippy::ptr_arg)]
fn unpack_rank_count(rc: &RankCount) -> Vec<u8> {
    let mut xs = Vec::with_capacity(5);
    for (r, freq) in rc {
        for _ in 0..*freq {
            xs.push(*r);
        }
    }
    xs
}

impl HandRank {
    fn find_straight(r: &RankFreq) -> Option<u8> {
        /*
         consecutive_count tracks number of 1s counting from current position (i)
         towards its right side.
        */
        let mut consecutive_count =
            // Treat Ace as if it was at position 14.
            if r[1] == 1 { 1 } else { 0 };
        for i in (1..=13).rev() {
            let cur = if r[i] == 1 { consecutive_count + 1 } else { 0 };
            if cur == 5 {
                return Some((i + 4) as u8);
            }
            consecutive_count = cur;
        }
        None
    }

    pub fn rank(cards: Vec<Card>) -> HandRank {
        let uniq_suits = {
            let mut xs = cards.iter().map(|c| c.suit).collect::<Vec<u8>>();
            xs.sort_unstable();
            xs.dedup();
            xs.len()
        };
        let rank_freq: RankFreq = {
            let mut freq = [0; 14];
            for c in cards {
                freq[c.rank.0 as usize] += 1;
            }
            freq
        };
        let straight = HandRank::find_straight(&rank_freq);
        let rank_count = to_rank_count(&rank_freq);

        if uniq_suits == 1 {
            // This is a flush.
            return match straight {
                Some(v) => HandRank::StraightFlush(v),
                None => HandRank::Flush(unpack_rank_count(&rank_count)),
            };
        }
        if let Some(v) = straight {
            // Not a flush but a straight
            return HandRank::Straight(v);
        }
        let rank_count = to_rank_count(&rank_freq);

        match rank_count.len() {
            2 => {
                // Only 2 distinct ranks, meaning Full house or Four of a kind.
                match rank_count[0].1 {
                    4 => HandRank::FourOfAKind(rank_count[0].0, rank_count[1].0),
                    3 => HandRank::FullHouse(rank_count[0].0, rank_count[1].0),
                    _ => unreachable!(),
                }
            }
            3 => {
                // Only 3 distinct ranks.
                match rank_count[0].1 {
                    3 => HandRank::ThreeOfAKind(rank_count[0].0, rank_count[1].0, rank_count[2].0),
                    2 => HandRank::TwoPair(rank_count[0].0, rank_count[1].0, rank_count[2].0),
                    _ => unreachable!(),
                }
            }
            4 => {
                // Must be exactly one pair.
                HandRank::OnePair(
                    rank_count[0].0,
                    rank_count[1].0,
                    rank_count[2].0,
                    rank_count[3].0,
                )
            }
            _ => HandRank::HighCard(unpack_rank_count(&rank_count)),
        }
    }
}

/// Given a list of poker hands, return a list of those hands which win.
///
/// Note the type signature: this function should return _the same_ reference to
/// the winning hand(s) as were passed in, not reconstructed strings which happen to be equal.
pub fn winning_hands<'a>(hands_raw: &[&'a str]) -> Option<Vec<&'a str>> {
    if hands_raw.is_empty() {
        return Some(vec![]);
    }
    let hands = {
        let mut xs = Vec::with_capacity(hands_raw.len());
        for x in hands_raw.iter() {
            xs.push(Hand::parse(x)?);
        }
        xs.sort_unstable_by(|x, y| y.rank.cmp(&x.rank));
        xs
    };
    Some(
        hands
            .iter()
            .take_while(|x| x.rank == hands[0].rank)
            .map(|x| x.src)
            .collect(),
    )
}
