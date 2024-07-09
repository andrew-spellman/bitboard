use crate::position::Position;

#[derive(Clone, Copy, PartialEq)]
enum Player {
    Black,
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

#[derive(Clone, Copy, PartialEq)]
enum Piece {
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

#[derive(Clone, Copy, PartialEq)]
struct Move {
    piece: Piece,
    origin: Position,
    destination: Position,
}

impl Move {
    fn new(piece: Piece, origin: Position, destination: Position) -> Self {
        Self {
            piece,
            origin,
            destination,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
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

impl Default for Board {
    fn default() -> Self {
        Self {
            black_pawn: 0xff000000000000,
            white_pawn: 0xff00,
            black_knight: 0x4200000000000000,
            white_knight: 0x42,
            black_bishop: 0x2400000000000000,
            white_bishop: 0x24,
            black_rook: 0x8100000000000000,
            white_rook: 0x81,
            black_queen: 0x800000000000000,
            white_queen: 0x8,
            black_king: 0x1000000000000000,
            white_king: 0x10,
            turn: Player::White,
            black_castle_kingside: true,
            white_castle_kingside: true,
            black_castle_queenside: true,
            white_castle_queenside: true,
            en_passant: None,
            halfmove_clock: 0,
            fullmove_count: 1,
        }
    }
}

impl std::fmt::Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_fen())
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for rank in (0..8).rev() {
            write!(f, "{} ", rank)?;
            for file in 0..8 {
                if let Some(piece) = self.piece_at(Position::from_file_rank(file, rank).unwrap()) {
                    write!(f, "{} ", piece)?;
                } else if (file + rank) % 2 == 0 {
                    write!(f, "# ")?;
                } else {
                    write!(f, "  ")?;
                }
            }
            write!(f, "\n")?;
        }
        write!(f, "  a b c d e f g h")?;
        Ok(())
    }
}

impl Board {
    fn empty() -> Self {
        Self {
            black_pawn: 0,
            white_pawn: 0,
            black_knight: 0,
            white_knight: 0,
            black_bishop: 0,
            white_bishop: 0,
            black_rook: 0,
            white_rook: 0,
            black_queen: 0,
            white_queen: 0,
            black_king: 0,
            white_king: 0,
            turn: Player::White,
            black_castle_kingside: false,
            white_castle_kingside: false,
            black_castle_queenside: false,
            white_castle_queenside: false,
            en_passant: None,
            halfmove_clock: 0,
            fullmove_count: 1,
        }
    }

    fn from_fen(fen: &str) -> Option<Self> {
        let mut b = Self::empty();
        let mut split = fen.split(" ");

        {
            let mut fen_positions = (0..8)
                .flat_map(|i| {
                    let left_rank = (7 - i) * 8;
                    (0..8).map(move |file| file + left_rank)
                })
                .peekable();

            for c in split.next()?.chars() {
                match c {
                    '/' => {
                        if fen_positions.peek()? % 8 != 0 {
                            return None;
                        }
                    }
                    c if c.is_digit(9) => {
                        let skip_count = c.to_digit(9).unwrap() as usize;
                        if skip_count == 0 {
                            return None;
                        }
                        if fen_positions.peek()? % 8 + skip_count > 8 {
                            return None;
                        }
                        for _ in 0..skip_count {
                            _ = fen_positions.next()?;
                        }
                    }
                    c => {
                        use Piece::*;
                        use Player::*;
                        b.set(
                            match c {
                                'p' => Pawn(Black),
                                'P' => Pawn(White),
                                'n' => Knight(Black),
                                'N' => Knight(White),
                                'b' => Bishop(Black),
                                'B' => Bishop(White),
                                'r' => Rook(Black),
                                'R' => Rook(White),
                                'q' => Queen(Black),
                                'Q' => Queen(White),
                                'k' => King(Black),
                                'K' => King(White),
                                _ => return None,
                            },
                            Position::from_index(fen_positions.next()?).unwrap(),
                        )
                    }
                }
            }
        }

        b.turn = match split.next()?.chars().next()? {
            'b' | 'B' => Some(Player::Black),
            'w' | 'W' => Some(Player::White),
            _ => None,
        }?;
        {
            let castlings = split.next()?;
            b.black_castle_kingside = castlings.contains('k');
            b.black_castle_queenside = castlings.contains('q');
            b.white_castle_kingside = castlings.contains('K');
            b.white_castle_queenside = castlings.contains('Q');
        }
        b.en_passant = match split.next()? {
            "-" => None,
            algebraic => Some(Position::from_algebraic(algebraic)?),
        };
        b.halfmove_clock = split.next()?.parse().ok()?;
        b.fullmove_count = split.next()?.parse().ok()?;
        Some(b)
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

    fn to_fen(&self) -> String {
        let mut fen = String::with_capacity(87);
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
        if !(self.black_castle_queenside
            || self.black_castle_kingside
            || self.white_castle_queenside
            || self.white_castle_kingside)
        {
            fen.push('-');
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
        let second = {
            match player {
                Player::Black => p.to_file_rank().1 == 6,
                Player::White => p.to_file_rank().1 == 1,
            }
            .then(|| {
                first.and_then(|first| {
                    first
                        .moved_by(0, forward)
                        .filter(|&second| !self.piece_at(second).is_some())
                })
            })
            .flatten()
        };
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
        use Player::*;
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
            .chain({
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
            })
    }

    fn pseudo_legal_moves<'a>(
        &'a self,
        player: Player,
        piece: Piece,
        p: Position,
    ) -> impl Iterator<Item = Position> + 'a {
        use Piece::*;
        match piece {
            Pawn(_) => Box::new(self.pseudo_legal_pawn_moves(player, p))
                as Box<dyn Iterator<Item = Position>>,
            Knight(_) => Box::new(self.pseudo_legal_knight_moves(player, p)),
            Bishop(_) => Box::new(self.pseudo_legal_bishop_moves(player, p)),
            Rook(_) => Box::new(self.pseudo_legal_rook_moves(player, p)),
            Queen(_) => Box::new(
                self.pseudo_legal_rook_moves(player, p)
                    .chain(self.pseudo_legal_bishop_moves(player, p)),
            ),
            King(_) => Box::new(self.pseudo_legal_king_moves(player, p)),
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

    fn clear(&mut self, piece: Piece, p: Position) {
        *self.bitboard(piece) &= !p.to_one_hot();
    }

    fn set(&mut self, piece: Piece, p: Position) {
        *self.bitboard(piece) |= p.to_one_hot();
    }

    fn try_pseudo_legal_move(&self, m: Move) -> Option<Board> {
        use Piece::*;
        use Player::*;

        let mut b = *self;
        let mut en_passant = None;

        match m.piece {
            Pawn(Black) if Some(m.destination) == b.en_passant => {
                b.clear(Pawn(White), m.destination.moved_by(0, 1).unwrap());
            }
            Pawn(White) if Some(m.destination) == b.en_passant => {
                b.clear(Pawn(Black), m.destination.moved_by(0, -1).unwrap());
            }
            Pawn(Black) if (m.destination - m.origin).1.abs() == 2 => {
                en_passant = Some(m.destination.moved_by(0, 1).unwrap());
            }
            Pawn(White) if (m.destination - m.origin).1.abs() == 2 => {
                en_passant = Some(m.destination.moved_by(0, -1).unwrap());
            }
            Rook(Black) if m.origin.to_file_rank().0 == 0 => b.black_castle_queenside = false,
            Rook(White) if m.origin.to_file_rank().0 == 0 => b.black_castle_queenside = false,
            Rook(Black) if m.origin.to_file_rank().0 == 7 => b.black_castle_kingside = false,
            Rook(White) if m.origin.to_file_rank().0 == 7 => b.black_castle_kingside = false,
            King(Black) if (m.destination - m.origin).0 == -2 => {
                b.clear(Rook(Black), Position::from_file_rank(0, 7).unwrap());
                b.clear(Rook(Black), Position::from_file_rank(3, 7).unwrap());
            }
            King(White) if (m.destination - m.origin).0 == -2 => {
                b.clear(Rook(White), Position::from_file_rank(0, 0).unwrap());
                b.clear(Rook(White), Position::from_file_rank(3, 0).unwrap());
            }
            King(Black) if (m.destination - m.origin).0 == 2 => {
                b.clear(Rook(Black), Position::from_file_rank(7, 7).unwrap());
                b.clear(Rook(Black), Position::from_file_rank(5, 7).unwrap());
            }
            King(White) if (m.destination - m.origin).0 == 2 => {
                b.clear(Rook(White), Position::from_file_rank(7, 0).unwrap());
                b.clear(Rook(White), Position::from_file_rank(5, 0).unwrap());
            }
            _ => (),
        }

        match m.piece {
            King(Black) => {
                b.black_castle_queenside = false;
                b.black_castle_kingside = false;
            }
            King(White) => {
                b.white_castle_queenside = false;
                b.white_castle_kingside = false;
            }
            _ => (),
        }

        b.en_passant = en_passant;
        b.clear(m.piece, m.origin);
        if let Some(taken) = b.piece_at(m.destination) {
            b.clear(taken, m.destination)
        }
        b.set(m.piece, m.destination);
        if b.turn == Black {
            b.fullmove_count += 1;
        }
        if b.is_in_check(b.turn) {
            return None;
        }
        b.turn = b.turn.other();
        Some(b)
    }

    fn moves<'a>(&'a self) -> impl Iterator<Item = (Move, Board)> + 'a {
        (0..64)
            .into_iter()
            .filter_map(|i| {
                let origin = Position::from_index(i).unwrap();
                self.piece_at(origin)
                    .filter(|piece| piece.player() == self.turn)
                    .map(|piece| (piece, origin))
            })
            .flat_map(|(piece, origin)| {
                self.pseudo_legal_moves(self.turn, piece, origin)
                    .map(move |destination| (Move::new(piece, origin, destination)))
                    .flat_map(|m| self.try_pseudo_legal_move(m).map(|board| (m, board)))
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_is_start() {
        assert_eq!(
            &Board::default(),
            &Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
        )
    }

    #[test]
    fn from_to_fen() {
        assert_eq!(
            &Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
                .unwrap()
                .to_fen(),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        )
    }

    #[test]
    fn twenty_moves_from_default() {
        assert_eq!(&20, &Board::default().moves().count())
    }

    #[test]
    fn en_passant() {
        assert!(Board::from_fen("4k3/8/8/8/1p6/8/P7/4K3 w - - 0 1")
            .unwrap()
            .moves()
            .find_map(|(_, b)| (b.to_fen() == "4k3/8/8/8/Pp6/8/8/4K3 b - a3 0 1").then(|| b))
            .unwrap()
            .moves()
            .find(|(_, b)| b.to_fen() == "4k3/8/8/8/8/p7/8/4K3 w - - 0 2")
            .is_some())
    }
}
