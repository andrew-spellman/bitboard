use crate::board::Board;

use std::{collections::HashMap, rc::Rc};

struct Game {
    previous_board_counts: HashMap<Rc<Board>, usize>,
}

enum TreeNode {
    Ongoing,
    Checkmate(Rc<Board>),
    Stalemate(Rc<Board>),
    InsufficientMaterial(Rc<Board>),
    FiftyMoveRule(Rc<Board>),
    FiveFoldRepetition(Rc<Board>),
}

pub fn play_out_random_games(b: Board, count_games: usize) -> f32 {
    use crate::board::Status;
    use rand::seq::IteratorRandom;
    let player = b.turn;
    let mut win_count = 0f32;
    let mut rng = rand::thread_rng();
    for i in 0..count_games {
        let mut b = b;
        win_count += loop {
            if let Some(b_rng) = b.moves().choose(&mut rng).map(|(_, b)| b) {
                b = b_rng;
            }
            match b.status() {
                Status::Ongoing => (),
                Status::Checkmate => {
                    if b.turn == player {
                        break -1.0;
                    } else {
                        break 1.0;
                    }
                }
                _ => break 0.5,
            }
        }
    }
    win_count as f32 / count_games as f32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fuck() {
        println!("{}", play_out_random_games(Board::default(), 100));
        panic!()
    }
}
