#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position(u8);

use std::fmt;

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (file, rank) = self.to_algebraic();
        write!(f, "{}{}", file, rank)
    }
}

impl std::ops::Sub for Position {
    type Output = (isize, isize);
    fn sub(self, rhs: Self) -> Self::Output {
        let (lhs_file, lhs_rank) = {
            let (file, rank) = self.to_file_rank();
            (file as isize, rank as isize)
        };
        let (rhs_file, rhs_rank) = {
            let (file, rank) = rhs.to_file_rank();
            (file as isize, rank as isize)
        };
        (lhs_file - rhs_file, lhs_rank - rhs_rank)
    }
}

impl Position {
    pub fn from_index(pos: usize) -> Option<Self> {
        match pos {
            0..=63 => Some(Self(pos as u8)),
            _ => None,
        }
    }

    pub fn to_index(&self) -> usize {
        self.0 as usize
    }

    pub fn from_one_hot(pos: u64) -> Option<Self> {
        match pos.count_ones() {
            1 => Some(Self(pos.trailing_zeros() as u8)),
            _ => None,
        }
    }

    pub fn to_one_hot(&self) -> u64 {
        1 << self.0
    }

    pub fn from_file_rank(file: usize, rank: usize) -> Option<Self> {
        match (file, rank) {
            (0..=7, 0..=7) => Some(Self((file + rank * 8) as u8)),
            _ => None,
        }
    }

    pub fn to_file_rank(&self) -> (usize, usize) {
        (self.0 as usize % 8, self.0 as usize / 8)
    }

    pub fn from_algebraic(algebraic: &str) -> Option<Self> {
        let mut chars = algebraic.chars();
        let file = {
            let file = chars.next()?;
            match file {
                'a'..='z' => file as u32 - 'a' as u32,
                'A'..='Z' => file as u32 - 'A' as u32,
                _ => None?,
            }
        };
        let rank = {
            let rank = chars.next()?;
            match rank {
                '1'..='8' => rank as u32 - '1' as u32,
                _ => None?,
            }
        };
        Some(Self((file + rank * 8) as u8))
    }

    pub fn to_algebraic(&self) -> (char, char) {
        (
            char::from_u32(self.0 as u32 % 8 + 'a' as u32).unwrap(),
            char::from_u32(self.0 as u32 / 8 + '1' as u32).unwrap(),
        )
    }

    pub fn moved_by(self, delta_file: isize, delta_rank: isize) -> Option<Self> {
        let (new_file, new_rank) = {
            let (file, rank) = self.to_file_rank();
            (file as isize + delta_file, rank as isize + delta_rank)
        };
        Self::from_file_rank(new_file as usize, new_rank as usize)
    }

    pub fn is_dark(&self) -> bool {
        self.to_one_hot() & 0xAA55AA55AA55AA55 != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_to_algebraic() {
        let p = Position::from_algebraic("c4").unwrap();
        assert_eq!(p.to_algebraic(), ('c', '4'));
    }

    #[test]
    fn try_move() {
        let p = Position::from_algebraic("a1").unwrap();
        assert_eq!(p.moved_by(-1, 0), None);
        assert_eq!(p.moved_by(1, 0), Position::from_algebraic("b1"));
        assert_eq!(p, Position::from_algebraic("a1").unwrap());
    }
}
