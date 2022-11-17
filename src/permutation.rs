pub struct Permutations {
    current: u64,
    upper_bound: u64,
}

pub struct Permutation {
    flags: u64,
    upper_bound: u64,
}

impl Permutations {
    pub fn new(set_size: usize) -> Self {
        assert!(set_size < 64);
        Permutations {
            current: 0,
            upper_bound: 1 << set_size,
        }
    }
}

impl Iterator for Permutations {
    type Item = Permutation;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current == self.upper_bound {
            return None;
        }
        let current = self.current;
        self.current += 1;
        Some(Permutation {
            flags: current,
            upper_bound: self.upper_bound,
        })
    }
}

impl Iterator for Permutation {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        if self.upper_bound > 1 {
            self.upper_bound >>= 1;
            Some(match self.flags & self.upper_bound {
                0 => false,
                _ => true,
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn test_permutations() {
        let mut expected = HashSet::new();
        expected.insert(vec![false, false, false]);
        expected.insert(vec![false, false, true]);
        expected.insert(vec![false, true, false]);
        expected.insert(vec![false, true, true]);
        expected.insert(vec![true, false, false]);
        expected.insert(vec![true, false, true]);
        expected.insert(vec![true, true, false]);
        expected.insert(vec![true, true, true]);

        let permutations = Permutations::new(3);
        let got: HashSet<Vec<bool>> = permutations.map(|perm| perm.collect()).collect();

        assert_eq!(got, expected);
    }
}
