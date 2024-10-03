use std::hash::Hash;

use crate::position::Position;

#[derive(Clone, Copy, PartialEq, Hash, Debug)]
pub enum Player {
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

#[derive(Clone, Copy, PartialEq, Debug)]
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

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Move {
    piece: Piece,
    initial: Position,
    terminal: Position,
    take: Option<Piece>,
    action: Option<Action>,
}

impl Move {
    fn new(piece: Piece, initial: Position, terminal: Position) -> Self {
        Self {
            piece,
            terminal,
            initial,
            take: None,
            action: None,
        }
    }

    fn with_take(mut self, taken: Option<Piece>) -> Self {
        self.take = taken;
        self
    }

    fn with_action(mut self, action: Option<Action>) -> Self {
        self.action = action;
        self
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
enum Action {
    Castle {
        initial: Position,
        terminal: Position,
    },
    DoubleStep(Position),
    EnPassant(Position),
    Promote(Piece),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Status {
    Ongoing,
    Checkmate,
    Stalemate,
    InsufficientMaterial,
    FiftyMoveRule,
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
    pub turn: Player,
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

impl Hash for Board {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.black_pawn.hash(state);
        self.black_pawn.hash(state);
        self.white_pawn.hash(state);
        self.black_knight.hash(state);
        self.white_knight.hash(state);
        self.black_bishop.hash(state);
        self.white_bishop.hash(state);
        self.black_rook.hash(state);
        self.white_rook.hash(state);
        self.black_queen.hash(state);
        self.white_queen.hash(state);
        self.black_king.hash(state);
        self.white_king.hash(state);
        self.turn.hash(state);
        self.black_castle_kingside.hash(state);
        self.white_castle_kingside.hash(state);
        self.black_castle_queenside.hash(state);
        self.white_castle_queenside.hash(state);
        self.en_passant.hash(state);
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
            write!(f, "{} ", rank + 1)?;
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

    fn positions_of_piece(&self, piece: Piece) -> impl Iterator<Item = Position> {
        use Piece::*;
        use Player::*;
        let bitboard = match piece {
            Pawn(Black) => self.black_pawn,
            Pawn(White) => self.white_pawn,
            Knight(Black) => self.black_knight,
            Knight(White) => self.white_knight,
            Bishop(Black) => self.black_bishop,
            Bishop(White) => self.white_bishop,
            Rook(Black) => self.black_rook,
            Rook(White) => self.white_rook,
            Queen(Black) => self.black_queen,
            Queen(White) => self.white_queen,
            King(Black) => self.black_king,
            King(White) => self.white_king,
        };
        (0..64).filter_map(move |i| Position::from_index(i).filter(|p| p.to_one_hot() == bitboard))
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
        let mut collided = false;
        std::iter::from_fn(move || {
            if collided {
                return None;
            }
            p = p.moved_by(delta_file, delta_rank)?;
            if self
                .piece_at(p)
                .is_some_and(|piece| piece.player() == player)
            {
                return None;
            }
            if self
                .piece_at(p)
                .is_some_and(|piece| piece.player() == player.other())
            {
                collided = true;
            }
            Some(p)
        })
    }

    fn knight_pattern<'a>(
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

    fn bishop_pattern<'a>(
        &'a self,
        player: Player,
        p: Position,
    ) -> impl Iterator<Item = Position> + 'a {
        self.positions_by_delta(player, p, -1, -1)
            .chain(self.positions_by_delta(player, p, 1, -1))
            .chain(self.positions_by_delta(player, p, -1, 1))
            .chain(self.positions_by_delta(player, p, 1, 1))
    }

    fn rook_pattern<'a>(
        &'a self,
        player: Player,
        p: Position,
    ) -> impl Iterator<Item = Position> + 'a {
        self.positions_by_delta(player, p, -1, 0)
            .chain(self.positions_by_delta(player, p, 1, 0))
            .chain(self.positions_by_delta(player, p, 0, -1))
            .chain(self.positions_by_delta(player, p, 0, 1))
    }

    fn king_pattern<'a>(
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

    fn pseudo_legal_pawn_moves<'a>(
        &'a self,
        player: Player,
        initial: Position,
    ) -> impl Iterator<Item = Move> + 'a {
        use Piece::*;
        use Player::*;

        let piece = Pawn(player);
        let forward = match player {
            Black => -1,
            White => 1,
        };

        let to_move = |terminal| Move::new(piece, initial, terminal);

        let try_take = |m: Move| m.with_take(self.piece_at(m.terminal));

        let try_promotion = |m: Move| {
            let opposite_back_rank = match player {
                Black => 0,
                White => 7,
            };
            if m.terminal.to_file_rank().1 == opposite_back_rank {
                let promote_to = |piece: Piece| m.with_action(Some(Action::Promote(piece)));
                Box::new(
                    Some(promote_to(Knight(player)))
                        .into_iter()
                        .chain(Some(promote_to(Bishop(player))))
                        .chain(Some(promote_to(Rook(player))))
                        .chain(Some(promote_to(Queen(player)))),
                ) as Box<dyn Iterator<Item = Move>>
            } else {
                Box::new(Some(m).into_iter())
            }
        };

        let mut first_successful = false;
        let p_first = initial.moved_by(0, forward);

        let first = p_first
            .filter(|&first| self.piece_at(first).is_none())
            .inspect(|_| first_successful = true)
            .map(to_move)
            .map(try_promotion)
            .unwrap_or(Box::new(None.into_iter()));

        let is_pawn_initial_rank = match player {
            Player::Black => initial.to_file_rank().1 == 6,
            Player::White => initial.to_file_rank().1 == 1,
        };

        let second = (first_successful && is_pawn_initial_rank)
            .then(|| {
                initial
                    .moved_by(0, 2 * forward)
                    .filter(|&second| self.piece_at(second).is_none())
                    .map(to_move)
                    .map(|m| {
                        Box::new(
                            Some(m.with_action(Some(Action::DoubleStep(p_first.unwrap()))))
                                .into_iter(),
                        )
                    })
            })
            .flatten()
            .unwrap_or(Box::new(None.into_iter()));

        let left = initial
            .moved_by(-1, forward)
            .filter(|&left| {
                self.piece_at(left)
                    .is_some_and(|left_piece| left_piece.player() == player.other())
            })
            .map(to_move)
            .map(try_take)
            .map(try_promotion)
            .unwrap_or(Box::new(None.into_iter()));

        let right = initial
            .moved_by(1, forward)
            .filter(|&right| {
                self.piece_at(right)
                    .is_some_and(|right_piece| right_piece.player() == player.other())
            })
            .map(to_move)
            .map(try_take)
            .map(try_promotion)
            .unwrap_or(Box::new(None.into_iter()));

        let en_passant = Box::new(
            self.en_passant
                .filter(|&en_passant| {
                    let (delta_file, delta_rank) = en_passant - initial;
                    delta_rank == forward && delta_file.abs() == 1
                })
                .map(to_move)
                .map(|m| {
                    m.with_action(Some(Action::EnPassant(
                        m.terminal.moved_by(0, -1 * forward).unwrap(),
                    )))
                })
                .into_iter(),
        );

        first
            .into_iter()
            .chain(second)
            .chain(left)
            .chain(right)
            .chain(en_passant)
    }

    fn pseudo_legal_king_moves<'a>(
        &'a self,
        player: Player,
        initial: Position,
    ) -> impl Iterator<Item = Move> + 'a {
        use Piece::*;
        use Player::*;

        let back_rank = match player {
            Black => 7,
            White => 0,
        };

        let castle_move = |side: isize, terminal_king: Position, terminal_rook: Position| {
            self.positions_by_delta(player.other(), initial, side, 0)
                .find(|&p| self.piece_at(p) == Some(Rook(player)))
                .map(|initial_rook| Move {
                    piece: King(player),
                    initial,
                    terminal: terminal_king,
                    take: None,
                    action: Some(Action::Castle {
                        initial: initial_rook,
                        terminal: terminal_rook,
                    }),
                })
        };

        let castle_queenside = match player {
            Black => self.black_castle_queenside,
            White => self.white_castle_queenside,
        }
        .then(|| {
            let terminal_king = Position::from_file_rank(2, back_rank).unwrap();
            let terminal_rook = Position::from_file_rank(3, back_rank).unwrap();
            castle_move(-1, terminal_king, terminal_rook)
        })
        .flatten();

        let castle_kingside = match player {
            Black => self.black_castle_kingside,
            White => self.white_castle_kingside,
        }
        .then(|| {
            let terminal_king = Position::from_file_rank(6, back_rank).unwrap();
            let terminal_rook = Position::from_file_rank(5, back_rank).unwrap();
            castle_move(1, terminal_king, terminal_rook)
        })
        .flatten();

        self.try_delta_from_position(player, initial, -1, -1)
            .into_iter()
            .chain(self.try_delta_from_position(player, initial, 0, -1))
            .chain(self.try_delta_from_position(player, initial, 1, -1))
            .chain(self.try_delta_from_position(player, initial, -1, 0))
            .chain(self.try_delta_from_position(player, initial, 0, 0))
            .chain(self.try_delta_from_position(player, initial, 1, 0))
            .chain(self.try_delta_from_position(player, initial, -1, 1))
            .chain(self.try_delta_from_position(player, initial, 0, 1))
            .chain(self.try_delta_from_position(player, initial, 1, 1))
            .map(move |terminal| {
                Move::new(King(player), initial, terminal).with_take(self.piece_at(terminal))
            })
            .chain(castle_queenside)
            .chain(castle_kingside)
    }

    fn pseudo_legal_moves<'a>(
        &'a self,
        player: Player,
        piece: Piece,
        initial: Position,
    ) -> impl Iterator<Item = Move> + 'a {
        use Piece::*;
        let to_move =
            move |terminal| Move::new(piece, initial, terminal).with_take(self.piece_at(terminal));
        match piece {
            Pawn(_) => Box::new(self.pseudo_legal_pawn_moves(player, initial))
                as Box<dyn Iterator<Item = Move>>,
            Knight(_) => Box::new(self.knight_pattern(player, initial).map(to_move)),
            Bishop(_) => Box::new(self.bishop_pattern(player, initial).map(to_move)),
            Rook(_) => Box::new(self.rook_pattern(player, initial).map(to_move)),
            Queen(_) => Box::new(
                self.rook_pattern(player, initial)
                    .chain(self.bishop_pattern(player, initial))
                    .map(to_move),
            ),
            King(_) => Box::new(self.pseudo_legal_king_moves(player, initial)),
        }
    }

    fn is_in_check(&self, player: Player) -> bool {
        use Piece::*;
        let p_king = Position::from_one_hot(match player {
            Player::Black => self.black_king,
            Player::White => self.white_king,
        })
        .unwrap();
        let forward = match player {
            Player::Black => -1,
            Player::White => 1,
        };
        self.try_delta_from_position(player, p_king, -1, forward)
            .filter(|&p| match self.piece_at(p) {
                Some(Pawn(_)) => true,
                _ => false,
            })
            .into_iter()
            .chain(
                self.try_delta_from_position(player, p_king, 1, forward)
                    .filter(|&p| match self.piece_at(p) {
                        Some(Pawn(_)) => true,
                        _ => false,
                    }),
            )
            .chain(
                self.knight_pattern(player, p_king)
                    .filter(|&p| match self.piece_at(p) {
                        Some(Knight(_)) => true,
                        _ => false,
                    }),
            )
            .chain(
                self.bishop_pattern(player, p_king)
                    .filter(|&p| match self.piece_at(p) {
                        Some(Bishop(_)) => true,
                        Some(Queen(_)) => true,
                        _ => false,
                    }),
            )
            .chain(
                self.rook_pattern(player, p_king)
                    .filter(|&p| match self.piece_at(p) {
                        Some(Rook(_)) => true,
                        Some(Queen(_)) => true,
                        _ => false,
                    }),
            )
            .chain(
                self.king_pattern(player, p_king)
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

    fn sufficient_material(&self) -> bool {
        let has_opposite_bishops = |player: Player| {
            let mut has_dark = false;
            let mut has_light = false;
            self.positions_of_piece(Piece::Bishop(player))
                .for_each(|p| {
                    if p.is_dark() {
                        has_dark = true;
                    } else {
                        has_light = true;
                    }
                });
            has_dark || has_light
        };
        (self.black_pawn
            | self.white_pawn
            | self.black_rook
            | self.white_rook
            | self.black_queen
            | self.white_queen)
            != 0
            || has_opposite_bishops(Player::Black)
            || has_opposite_bishops(Player::White)
            || (self.black_knight != 0 && self.black_bishop != 0)
            || (self.white_knight != 0 && self.white_bishop != 0)
            || self.black_knight.count_zeros() >= 3
            || self.white_knight.count_zeros() >= 3
    }

    pub fn status(&self) -> Status {
        if self.halfmove_clock == 50 {
            Status::FiftyMoveRule
        } else if !self.sufficient_material() {
            Status::InsufficientMaterial
        } else if self.moves().next().is_none() {
            match self.is_in_check(self.turn) {
                false => Status::Stalemate,
                true => Status::Checkmate,
            }
        } else {
            Status::Ongoing
        }
    }

    fn clear(&mut self, piece: Piece, p: Position) {
        *self.bitboard(piece) &= !p.to_one_hot();
    }

    fn set(&mut self, piece: Piece, p: Position) {
        *self.bitboard(piece) |= p.to_one_hot();
    }

    fn try_pseudo_legal_move(&self, m: Move) -> Option<Board> {
        let mut b = *self;
        b.en_passant = None;
        b.clear(m.piece, m.initial);
        b.set(m.piece, m.terminal);

        if let Some(taken) = m.take {
            b.clear(taken, m.terminal);
        }

        if let Some(action) = m.action {
            use Action::*;
            match action {
                Castle { initial, terminal } => {
                    let rook = Piece::Rook(self.turn);
                    b.clear(rook, initial);
                    b.set(rook, terminal);
                }
                DoubleStep(en_passant) => b.en_passant = Some(en_passant),
                EnPassant(p_taken) => b.clear(Pawn(self.turn.other()), p_taken),
                Promote(piece) => {
                    b.clear(m.piece, m.terminal);
                    b.set(piece, m.terminal);
                }
            }
        }

        if b.is_in_check(self.turn) {
            return None;
        }

        use Piece::*;
        use Player::*;

        match m.piece {
            King(Black) => {
                b.black_castle_queenside = false;
                b.black_castle_kingside = false;
            }
            King(White) => {
                b.white_castle_queenside = false;
                b.white_castle_kingside = false;
            }
            Rook(Black) => {
                if (m.initial - Position::from_one_hot(b.black_king).unwrap()).0 < 0 {
                    b.black_castle_queenside = false;
                } else {
                    b.black_castle_kingside = false;
                }
            }
            Rook(White) => {
                if (m.initial - Position::from_one_hot(b.white_king).unwrap()).0 < 0 {
                    b.white_castle_queenside = false;
                } else {
                    b.white_castle_kingside = false;
                }
            }
            _ => (),
        }

        let is_pawn = m.piece == Pawn(Black) || m.piece == Pawn(White);

        if is_pawn || m.take.is_some() {
            b.halfmove_clock = 0;
        } else {
            b.halfmove_clock += 1;
        }

        if b.turn == Player::Black {
            b.fullmove_count += 1;
        }

        b.turn = b.turn.other();

        Some(b)
    }

    pub fn moves<'a>(&'a self) -> impl Iterator<Item = (Move, Board)> + 'a {
        (0..64)
            .into_iter()
            .filter_map(|i| {
                let initial = Position::from_index(i).unwrap();
                self.piece_at(initial)
                    .filter(|piece| piece.player() == self.turn)
                    .map(|piece| (piece, initial))
            })
            .flat_map(|(piece, initial)| {
                self.pseudo_legal_moves(self.turn, piece, initial)
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
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        assert_eq!(&Board::from_fen(fen).unwrap().to_fen(), fen)
    }

    #[test]
    fn twenty_moves_from_default() {
        assert_eq!(
            &20,
            &Board::default()
                .moves()
                .inspect(|(m, b)| println!("{:?}", (m, b)))
                .count()
        );
    }

    impl Board {
        fn find_board(&self, fen: &str) -> Board {
            self.moves()
                .inspect(|(m, b)| println!("{:?}", (m, b)))
                .find_map(|(_, b)| (b.to_fen() == fen).then(|| b))
                .unwrap()
        }
    }

    #[test]
    fn en_passant() {
        Board::from_fen("4k3/8/8/8/1p6/8/P7/4K3 w - - 0 1")
            .unwrap()
            .find_board("4k3/8/8/8/Pp6/8/8/4K3 b - a3 0 1")
            .find_board("4k3/8/8/8/8/p7/8/4K3 w - - 0 2");
    }

    #[test]
    fn castling_queenside() {
        Board::from_fen("r3k3/8/8/8/8/8/8/R3K3 w Qq - 0 1")
            .unwrap()
            .find_board("r3k3/8/8/8/8/8/8/2KR4 b q - 1 1")
            .find_board("2kr4/8/8/8/8/8/8/2KR4 w - - 2 2");
    }

    #[test]
    fn castling_kingside() {
        Board::from_fen("4k2r/8/8/8/8/8/8/4K2R w Kk - 0 1")
            .unwrap()
            .find_board("4k2r/8/8/8/8/8/8/5RK1 b k - 1 1")
            .find_board("5rk1/8/8/8/8/8/8/5RK1 w - - 2 2");
    }

    #[test]
    fn in_check() {
        let assert_white_is_in_check = |fen: &str| {
            assert!(Board::from_fen(fen).unwrap().is_in_check(Player::White));
        };
        assert_white_is_in_check("4k3/8/8/8/8/8/3p4/4K3 w - - 0 1");
        assert_white_is_in_check("4k3/8/8/8/8/3n4/8/4K3 w - - 0 1");
        assert_white_is_in_check("4k3/8/8/8/1b6/8/8/4K3 w - - 0 1");
        assert_white_is_in_check("4k3/8/8/8/8/8/8/r3K3 w - - 0 1");
        assert_white_is_in_check("4k3/8/8/8/7q/8/8/4K3 w - - 0 1");
    }

    #[test]
    fn checkmate() {
        let b = Board::from_fen("8/8/8/8/8/4k3/8/r3K3 w - - 0 1").unwrap();
        assert_eq!(Status::Checkmate, b.status())
    }

    #[test]
    fn stalemate() {
        let b = Board::from_fen("8/8/8/8/8/4k3/4p3/4K3 w - - 0 1").unwrap();
        assert_eq!(Status::Stalemate, b.status())
    }
}
