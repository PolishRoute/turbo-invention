use std::ops::Range;
use std::path::Path;

trait StrSliceExt {
    fn split_until(&self, predicate: impl FnMut(char) -> bool) -> (&str, &str);
    fn take_until(&self, predicate: impl FnMut(char) -> bool) -> &str {
        self.split_until(predicate).0
    }
}

impl StrSliceExt for str {
    #[inline]
    fn split_until(&self, mut predicate: impl FnMut(char) -> bool) -> (&str, &str) {
        for (offset, char) in self.char_indices() {
            if !predicate(char) {
                return self.split_at(offset);
            }
        }
        (self, "")
    }
}

#[derive(Debug)]
pub enum Token<'s> {
    Ident(&'s str),
    String(&'s str),
    Number(i32),
    Space(char),
    Punct(char),
}

pub fn parse_line(line: &str) -> impl Iterator<Item=(Range<usize>, Token)> {
    let mut pos = 0;
    std::iter::from_fn(move || {
        let mut it = line[pos..].chars().peekable();
        let (len, token) = match (it.next()?, it.peek()) {
            ('0'..='9', _) | ('-', Some('0'..='9')) => {
                let number = line[pos..].take_until(|c| matches!(c, '-' | '0'..='9'));
                (number.len(), Token::Number(number.parse().unwrap()))
            }
            ('"', _) => {
                let string = line[pos + 1..].take_until(|c| c != '"');
                (string.len() + 2, Token::String(string))
            }
            (c, _) if c.is_whitespace() => {
                (c.len_utf8(), Token::Space(c))
            }
            ('_' | 'a'..='z' | 'A'..='Z', _) => {
                let punct = line[pos..].take_until(|c| matches!(c, '_' | '0'..='9' | 'a'..='z' | 'A'..='Z'));
                (punct.len(), Token::Ident(punct))
            }
            (c, _) => {
                (c.len_utf8(), Token::Punct(c))
            }
        };
        let span = pos..pos + len;
        pos += len;
        Some((span, token))
    })
}

#[derive(Debug)]
enum KeyPart {
    Ident(Box<str>),
    Index(u32),
}

#[derive(Debug)]
struct Entry {
    key: Vec<KeyPart>,
}

struct File {
    entries: Vec<Entry>,
}

impl File {
    fn find<const N: usize>(&self, path: [&'static str; N]) -> impl Iterator<Item=&'_ Entry> {
        self.entries
            .iter()
            .filter(move |e|
                e
                    .key
                    .iter()

                    .zip(path)
                    .all(|c| match c {
                        (KeyPart::Index(_), "$") => true,
                        (KeyPart::Ident(_), "*") => true,
                        (KeyPart::Ident(s), other) => &**s == other,
                        _ => false,
                    })
            )
    }
}

fn load_file(path: impl AsRef<Path>) -> File {
    let content = std::fs::read(path).unwrap();
    let content = encoding_rs::SHIFT_JIS.decode(&content).0;

    for line in content.lines().filter_map(|line| line.strip_prefix('#')) {
        let mut key = vec![];
        let mut in_group = false;
        for part in parse_line(line) {
            match part.1 {
                Token::Ident(name) => {
                    key.push(KeyPart::Ident(name.to_string().into_boxed_str()));
                }
                Token::Number(value) => {
                    key.push(KeyPart::Index(value as u32));
                }
                Token::String(_) => unimplemented!(),
                Token::Space(_) => {}
                Token::Punct('=') => {
                    break;
                }
                Token::Punct('.') => {}
                Token::Punct(',') if in_group => {}
                Token::Punct('(') => {
                    in_group = true;
                }
                Token::Punct(')') => {
                    in_group = false;
                }
                Token::Punct('[' | ']') => {}
                Token::Punct(x) => println!("xx: {}", x),
            }
        }

        println!("{:?}", key);
    }
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::gameini::{Token, load_file};

    #[test]
    fn foo() {
        load_file(r"C:\Users\Host\Downloads\Gameexe.ini");
    }
}