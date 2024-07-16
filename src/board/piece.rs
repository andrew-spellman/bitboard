use crate::board::Player;

#[derive(Clone, Copy, PartialEq)]
pub enum Piece {
    Pawn(Player),
    Knight(Player),
    Bishop(Player),
    Rook(Player),
    Queen(Player),
    King(Player),
}

impl std::fmt::Display for Piece {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Piece::*;
        use Player::*;
        write!(
            f,
            "{}",
            match self {
                Pawn(Black) => '♟',
                Pawn(White) => '♙',
                Knight(Black) => '♞',
                Knight(White) => '♘',
                Bishop(Black) => '♝',
                Bishop(White) => '♗',
                Rook(Black) => '♜',
                Rook(White) => '♖',
                Queen(Black) => '♛',
                Queen(White) => '♕',
                King(Black) => '♚',
                King(White) => '♔',
            }
        )
    }
}

impl Piece {
    pub fn char(&self) -> char {
        use Piece::*;
        use Player::*;
        match self {
            Pawn(Black) => 'p',
            Pawn(White) => 'P',
            Knight(Black) => 'n',
            Knight(White) => 'N',
            Bishop(Black) => 'b',
            Bishop(White) => 'B',
            Rook(Black) => 'r',
            Rook(White) => 'R',
            Queen(Black) => 'q',
            Queen(White) => 'Q',
            King(Black) => 'k',
            King(White) => 'K',
        }
    }

    pub fn player(&self) -> Player {
        use Piece::*;
        match self {
            Pawn(player) => *player,
            Knight(player) => *player,
            Bishop(player) => *player,
            Rook(player) => *player,
            Queen(player) => *player,
            King(player) => *player,
        }
    }
}
