use crate::position::Position;

#[derive(Debug, Default, Clone, Copy, PartialEq)]
pub enum Player {
    Black,
    #[default]
    White,
}

impl Player {
    fn other(&self) -> Self {
        match self {
            Player::Black => Player::White,
            Player::White => Player::Black,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Piece {
    Pawn(Player),
    Knight(Player),
    Bishop(Player),
    Rook(Player),
    Queen(Player),
    King(Player),
}

impl Piece {
    fn char(&self) -> char {
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

    fn player(&self) -> Player {
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

#[derive(Default, Clone, Copy)]
pub struct Board {
    black_pawn: u64,
    white_pawn: u64,
    black_knight: u64,
    white_knight: u64,
    black_bishop: u64,
    white_bishop: u64,
    black_rook: u64,
    white_rook: u64,
    black_queen: u64,
    white_queen: u64,
    black_king: u64,
    white_king: u64,
    turn: Player,
    black_castle_kingside: bool,
    white_castle_kingside: bool,
    black_castle_queenside: bool,
    white_castle_queenside: bool,
    en_passant: Option<Position>,
    halfmove_clock: usize,
    fullmove_count: usize,
}

impl Board {
    fn from_fen(fen: &str) -> Option<Self> {
        let mut board = Self::default();
        let mut split = fen.split(" ");

        let last_position = split
            .next()?
            .chars()
            .try_fold(
                (Position::from_file_rank(0, 7)?, false),
                |(p, end_of_rank), c| match c {
                    '/' if p.to_file_rank().1 == 0 => None,
                    '/' if !end_of_rank => None,
                    '/' => Some((p.moved_by(-7, -1).unwrap(), false)),
                    _ if end_of_rank => None,
                    c if c.is_ascii_digit() => {
                        let skip_count = c.to_digit(10).unwrap() - 1;
                        if skip_count == 0 {
                            return None;
                        }
                        if p.to_file_rank().0 + skip_count as usize > 7 {
                            return None;
                        }
                        let p = p.moved_by(skip_count as isize, 0).unwrap();
                        if p.to_file_rank().0 == 7 {
                            Some((p, true))
                        } else {
                            Some((p, false))
                        }
                    }
                    c => {
                        *match c {
                            'p' => &mut board.black_pawn,
                            'P' => &mut board.white_pawn,
                            'n' => &mut board.black_knight,
                            'N' => &mut board.white_knight,
                            'b' => &mut board.black_bishop,
                            'B' => &mut board.white_bishop,
                            'r' => &mut board.black_rook,
                            'R' => &mut board.white_rook,
                            'q' => &mut board.black_queen,
                            'Q' => &mut board.white_queen,
                            'k' => &mut board.black_king,
                            'K' => &mut board.white_king,
                            _ => return None,
                        } |= p.to_one_hot();
                        match p.to_file_rank() {
                            (7, _) => Some((p, true)),
                            _ => Some((p.moved_by(1, 0).unwrap(), false)),
                        }
                    }
                },
            )?
            .0;
        if last_position.to_index() != 7 {
            return None;
        }
        board.turn = match split.next()?.chars().next()? {
            'b' | 'B' => Some(Player::Black),
            'w' | 'W' => Some(Player::White),
            _ => None,
        }?;
        {
            let castlings = split.next()?;
            board.black_castle_kingside = castlings.contains('k');
            board.black_castle_queenside = castlings.contains('q');
            board.white_castle_kingside = castlings.contains('K');
            board.white_castle_queenside = castlings.contains('Q');
        }
        board.en_passant = match split.next()? {
            "-" => None,
            algebraic => Some(Position::from_algebraic(algebraic)?),
        };
        board.halfmove_clock = split.next()?.parse().ok()?;
        board.fullmove_count = split.next()?.parse().ok()?;
        Some(board)
    }

    fn piece_at(&self, p: Position) -> Option<Piece> {
        use Piece::*;
        use Player::*;
        let p = p.to_one_hot();
        match () {
            _ if self.black_pawn & p != 0 => Some(Pawn(Black)),
            _ if self.white_pawn & p != 0 => Some(Pawn(White)),
            _ if self.black_knight & p != 0 => Some(Knight(Black)),
            _ if self.white_knight & p != 0 => Some(Knight(White)),
            _ if self.black_bishop & p != 0 => Some(Bishop(Black)),
            _ if self.white_bishop & p != 0 => Some(Bishop(White)),
            _ if self.black_rook & p != 0 => Some(Rook(Black)),
            _ if self.white_rook & p != 0 => Some(Rook(White)),
            _ if self.black_queen & p != 0 => Some(Queen(Black)),
            _ if self.white_queen & p != 0 => Some(Queen(White)),
            _ if self.black_king & p != 0 => Some(King(Black)),
            _ if self.white_king & p != 0 => Some(King(White)),
            _ => None,
        }
    }

    fn bitboard<'a>(&'a mut self, piece: Piece) -> &'a mut u64 {
        use Piece::*;
        use Player::*;
        match piece {
            Pawn(Black) => &mut self.black_pawn,
            Pawn(White) => &mut self.white_pawn,
            Knight(Black) => &mut self.black_knight,
            Knight(White) => &mut self.white_knight,
            Bishop(Black) => &mut self.black_bishop,
            Bishop(White) => &mut self.white_bishop,
            Rook(Black) => &mut self.black_rook,
            Rook(White) => &mut self.white_rook,
            Queen(Black) => &mut self.black_queen,
            Queen(White) => &mut self.white_queen,
            King(Black) => &mut self.black_king,
            King(White) => &mut self.white_king,
        }
    }

    pub fn to_fen(&self) -> String {
        let mut fen = String::new();
        for rank in (0..8).rev() {
            let mut consecutive_empty = 0u32;
            for file in 0..8 {
                let p = Position::from_file_rank(file, rank).unwrap();
                let piece = self.piece_at(p);
                if let Some(piece) = piece {
                    if consecutive_empty > 0 {
                        fen.push(char::from_digit(consecutive_empty, 10).unwrap());
                        consecutive_empty = 0;
                    }
                    fen.push(piece.char());
                } else {
                    consecutive_empty += 1;
                }
            }
            if consecutive_empty > 0 {
                fen.push(char::from_digit(consecutive_empty, 10).unwrap());
            }
            fen.push('/');
        }
        fen.pop();
        fen.push(' ');
        match self.turn {
            Player::Black => fen.push('b'),
            Player::White => fen.push('w'),
        }
        fen.push(' ');
        if self.white_castle_kingside {
            fen.push('K');
        }
        if self.white_castle_queenside {
            fen.push('Q');
        }
        if self.black_castle_kingside {
            fen.push('k');
        }
        if self.black_castle_queenside {
            fen.push('q');
        }
        fen.push(' ');
        if let Some(en_passant) = &self.en_passant {
            let algebraic = en_passant.to_algebraic();
            fen.push(algebraic.0);
            fen.push(algebraic.1);
        } else {
            fen.push('-');
        }
        fen.push(' ');
        fen.push_str(&self.halfmove_clock.to_string());
        fen.push(' ');
        fen.push_str(&self.fullmove_count.to_string());
        fen
    }

    fn try_delta_from_position<'a>(
        &'a self,
        player: Player,
        p: Position,
        delta_file: isize,
        delta_rank: isize,
    ) -> Option<Position> {
        if self
            .piece_at(p)
            .is_some_and(|piece| piece.player() == player.other())
        {
            return None;
        }
        let p = p.moved_by(delta_file, delta_rank)?;
        if self
            .piece_at(p)
            .is_some_and(|piece| piece.player() == player)
        {
            return None;
        }
        Some(p)
    }

    fn positions_by_delta<'a>(
        &'a self,
        player: Player,
        mut p: Position,
        delta_file: isize,
        delta_rank: isize,
    ) -> impl Iterator<Item = Position> + 'a {
        std::iter::from_fn(move || {
            p = self.try_delta_from_position(player, p, delta_file, delta_rank)?;
            Some(p)
        })
    }

    fn pseudo_legal_pawn_moves<'a>(
        &'a self,
        player: Player,
        p: Position,
    ) -> impl Iterator<Item = Position> + 'a {
        let forward = match player {
            Player::Black => -1,
            Player::White => 1,
        };
        let first = p
            .moved_by(0, forward)
            .filter(|&first| !self.piece_at(first).is_some());
        let second = first.and_then(|first| {
            first
                .moved_by(0, forward)
                .filter(|&second| !self.piece_at(second).is_some())
        });
        let left = p.moved_by(-1, forward).filter(|&left| {
            self.piece_at(left)
                .is_some_and(|left_piece| left_piece.player() == player.other())
        });
        let right = p.moved_by(1, forward).filter(|&right| {
            self.piece_at(right)
                .is_some_and(|right_piece| right_piece.player() == player.other())
        });
        let en_passant = self.en_passant.filter(|&en_passant| {
            let (delta_file, delta_rank) = en_passant - p;
            delta_rank == forward && delta_file.abs() == 1
        });
        first
            .into_iter()
            .chain(second)
            .chain(left)
            .chain(right)
            .chain(en_passant)
    }

    fn pseudo_legal_knight_moves<'a>(
        &'a self,
        player: Player,
        p: Position,
    ) -> impl Iterator<Item = Position> + 'a {
        self.try_delta_from_position(player, p, -1, -2)
            .into_iter()
            .chain(self.try_delta_from_position(player, p, 1, -2))
            .chain(self.try_delta_from_position(player, p, -2, -1))
            .chain(self.try_delta_from_position(player, p, 2, -1))
            .chain(self.try_delta_from_position(player, p, -2, 1))
            .chain(self.try_delta_from_position(player, p, 2, 1))
            .chain(self.try_delta_from_position(player, p, -1, 2))
            .chain(self.try_delta_from_position(player, p, 1, 2))
    }

    fn pseudo_legal_bishop_moves<'a>(
        &'a self,
        player: Player,
        p: Position,
    ) -> impl Iterator<Item = Position> + 'a {
        self.positions_by_delta(player, p, -1, -1)
            .chain(self.positions_by_delta(player, p, 1, -1))
            .chain(self.positions_by_delta(player, p, -1, 1))
            .chain(self.positions_by_delta(player, p, 1, 1))
    }

    fn pseudo_legal_rook_moves<'a>(
        &'a self,
        player: Player,
        p: Position,
    ) -> impl Iterator<Item = Position> + 'a {
        self.positions_by_delta(player, p, -1, 0)
            .chain(self.positions_by_delta(player, p, 1, 0))
            .chain(self.positions_by_delta(player, p, 0, -1))
            .chain(self.positions_by_delta(player, p, 0, 1))
    }

    fn pseudo_legal_king_moves<'a>(
        &'a self,
        player: Player,
        p: Position,
    ) -> impl Iterator<Item = Position> + 'a {
        self.try_delta_from_position(player, p, -1, -1)
            .into_iter()
            .chain(self.try_delta_from_position(player, p, 0, -1))
            .chain(self.try_delta_from_position(player, p, 1, -1))
            .chain(self.try_delta_from_position(player, p, -1, 0))
            .chain(self.try_delta_from_position(player, p, 0, 0))
            .chain(self.try_delta_from_position(player, p, 1, 0))
            .chain(self.try_delta_from_position(player, p, -1, 1))
            .chain(self.try_delta_from_position(player, p, 0, 1))
            .chain(self.try_delta_from_position(player, p, 1, 1))
    }

    fn pseudo_legal_castling_moves<'a>(
        &'a self,
        player: Player,
    ) -> impl Iterator<Item = Position> + 'a {
        use Player::*;
        match player {
            Black => self
                .black_castle_queenside
                .then(|| {
                    (self
                        .piece_at(Position::from_algebraic("d8").unwrap())
                        .is_none()
                        && self
                            .piece_at(Position::from_algebraic("c8").unwrap())
                            .is_none()
                        && self
                            .piece_at(Position::from_algebraic("b8").unwrap())
                            .is_none())
                    .then(|| Position::from_algebraic("c8").unwrap())
                })
                .flatten()
                .into_iter()
                .chain({
                    self.black_castle_kingside
                        .then(|| {
                            (self
                                .piece_at(Position::from_algebraic("f8").unwrap())
                                .is_none()
                                && self
                                    .piece_at(Position::from_algebraic("g8").unwrap())
                                    .is_none())
                            .then(|| Position::from_algebraic("e8").unwrap())
                        })
                        .flatten()
                }),
            White => self
                .white_castle_queenside
                .then(|| {
                    (self
                        .piece_at(Position::from_algebraic("d1").unwrap())
                        .is_none()
                        && self
                            .piece_at(Position::from_algebraic("c1").unwrap())
                            .is_none()
                        && self
                            .piece_at(Position::from_algebraic("b1").unwrap())
                            .is_none())
                    .then(|| Position::from_algebraic("c1").unwrap())
                })
                .flatten()
                .into_iter()
                .chain({
                    self.white_castle_kingside
                        .then(|| {
                            (self
                                .piece_at(Position::from_algebraic("f1").unwrap())
                                .is_none()
                                && self
                                    .piece_at(Position::from_algebraic("g1").unwrap())
                                    .is_none())
                            .then(|| Position::from_algebraic("e1").unwrap())
                        })
                        .flatten()
                }),
        }
    }

    fn is_in_check(&self, player: Player) -> bool {
        use Piece::*;
        let king = Position::from_one_hot(match player {
            Player::Black => self.black_king,
            Player::White => self.white_king,
        })
        .unwrap();
        let forward = match player {
            Player::Black => -1,
            Player::White => 1,
        };
        self.try_delta_from_position(player, king, -1, forward)
            .into_iter()
            .chain(self.try_delta_from_position(player, king, -1, forward))
            .chain(self.pseudo_legal_knight_moves(player, king).filter(
                |&p| match self.piece_at(p) {
                    Some(Knight(_)) => true,
                    _ => false,
                },
            ))
            .chain(self.pseudo_legal_bishop_moves(player, king).filter(
                |&p| match self.piece_at(p) {
                    Some(Bishop(_)) => true,
                    Some(Queen(_)) => true,
                    _ => false,
                },
            ))
            .chain(
                self.pseudo_legal_rook_moves(player, king)
                    .filter(|&p| match self.piece_at(p) {
                        Some(Rook(_)) => true,
                        Some(Queen(_)) => true,
                        _ => false,
                    }),
            )
            .chain(
                self.pseudo_legal_king_moves(player, king)
                    .filter(|&p| match self.piece_at(p) {
                        Some(King(_)) => true,
                        _ => false,
                    }),
            )
            .find(|&p| {
                self.piece_at(p)
                    .is_some_and(|piece| piece.player() == player.other())
            })
            .is_some()
    }

    fn moves<'a>(&'a self) -> impl Iterator<Item = Board> + 'a {
        use Piece::*;
        (0..64)
            .into_iter()
            .filter_map(|i| {
                let origin = Position::from_index(i).unwrap();
                self.piece_at(origin)
                    .filter(|piece| piece.player() == self.turn)
                    .map(|piece| (piece, origin))
            })
            .map(|(piece, origin)| {
                match piece {
                    Pawn(_) => Box::new(self.pseudo_legal_pawn_moves(self.turn, origin))
                        as Box<dyn Iterator<Item = Position>>,
                    Knight(_) => Box::new(self.pseudo_legal_knight_moves(self.turn, origin)),
                    Bishop(_) => Box::new(self.pseudo_legal_bishop_moves(self.turn, origin)),
                    Rook(_) => Box::new(self.pseudo_legal_rook_moves(self.turn, origin)),
                    Queen(_) => Box::new(
                        self.pseudo_legal_rook_moves(self.turn, origin)
                            .chain(self.pseudo_legal_bishop_moves(self.turn, origin)),
                    ),
                    King(_) => Box::new(self.pseudo_legal_king_moves(self.turn, origin)),
                }
                .map(move |destination| (piece, origin, destination))
            })
            .flatten()
            .map(|(piece, origin, destination)| {
                let mut b = *self;
                *b.bitboard(piece) &= !origin.to_one_hot();
                if let Some(taken) = b.piece_at(destination) {
                    assert_eq!(taken.player(), b.turn.other());
                    *b.bitboard(taken) &= !destination.to_one_hot();
                }
                *b.bitboard(piece) |= destination.to_one_hot();
                b.turn = b.turn.other();
                b
            })
            .filter(|b| b.is_in_check(b.turn.other()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_to_fen() {
        let b =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
        assert_eq!(
            &b.to_fen(),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        );
    }

    #[test]
    fn dumb() {
        let b =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
        assert!(!b.is_in_check(Player::White));
        b.moves().for_each(|b| {
            println!("{}", b.to_fen());
        });
        panic!();
    }
}