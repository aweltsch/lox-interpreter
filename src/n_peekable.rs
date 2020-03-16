use std::collections::VecDeque;
use std::option::Option;
use std::str::Chars;

pub struct NPeekable<'a> {
    lookup: VecDeque<char>,
    char_iter: Chars<'a> }

    impl<'a> NPeekable<'a> {
        pub fn new(s: &str) -> NPeekable {
            NPeekable { lookup: VecDeque::new(), char_iter: s.chars() }
        }

        pub fn next(&mut self) -> Option<char> {
            if self.lookup.len() > 0 {
                self.lookup.pop_front()
            } else {
                self.char_iter.next()
            }
        }

        pub fn peek(&mut self) -> Option<&char> {
            self.n_peek(1)
        }

        pub fn n_peek(&mut self, n: usize) -> Option<&char> {
            if n == 0 {
                return None;
            }
            if n > self.lookup.len() {
                for i in 0..(n - self.lookup.len()) {
                    self.lookup.push_back(self.char_iter.next()?);
                }
            }
            self.lookup.get(n - 1)
        }
    }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn n_peekable() {
        let mut char_iter = NPeekable::new("012345");
        assert_eq!(char_iter.n_peek(0).is_none(), true);

        assert_eq!(char_iter.peek().unwrap(), &'0');
        assert_eq!(char_iter.n_peek(1).unwrap(), &'0');
        assert_eq!(char_iter.n_peek(3).unwrap(), &'2');
        assert_eq!(char_iter.n_peek(100).is_none(), true);

        assert_eq!(char_iter.next().unwrap(), '0');
        assert_eq!(char_iter.next().unwrap(), '1');
        assert_eq!(char_iter.next().unwrap(), '2');

        assert_eq!(char_iter.peek().unwrap(), &'3');
        assert_eq!(char_iter.n_peek(2).unwrap(), &'4');
        assert_eq!(char_iter.peek().unwrap(), &'3');

        assert_eq!(char_iter.next().unwrap(), '3');
        assert_eq!(char_iter.next().unwrap(), '4');
        assert_eq!(char_iter.next().unwrap(), '5');
        assert_eq!(char_iter.next().is_none(), true);
        assert_eq!(char_iter.next().is_none(), true);

        assert_eq!(char_iter.peek().is_none(), true);
        assert_eq!(char_iter.n_peek(2).is_none(), true);
    }
}
