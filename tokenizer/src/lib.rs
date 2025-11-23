//! SQL tokenizer

use std::iter::Peekable;
use std::str;
use std::str::Chars;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    NumericLiteral(&'a str),
    StringLiteral(&'a str),
    // Keyword
    Create,
    Table,
    Select,
    From,
    Where,
    And,
    Or,
    // Operator
    Semicolon,
    Comma,
    Equals,
    LP,
    RP,
    // Invalid
    InvalidCharacter(char),
}

fn identify_keyword_or_identifier<'a>(literal: &'a str) -> Token<'a> {
    let mut buf = [0u8; 32];
    if literal.len() > buf.len() {
        return Token::Identifier(literal); // This is fine because we don't support keywords longer than 32 chars
    }
    buf[..literal.len()].copy_from_slice(literal.as_bytes());
    buf[..literal.len()].make_ascii_uppercase();
    match str::from_utf8(&buf[..literal.len()]) {
        Ok(upper_literal_copy) => match upper_literal_copy {
            "CREATE" => Token::Create,
            "TABLE" => Token::Table,
            "SELECT" => Token::Select,
            "FROM" => Token::From,
            "WHERE" => Token::Where,
            "AND" => Token::And,
            "OR" => Token::Or,
            _ => Token::Identifier(literal),
        },
        Err(_) => Token::Identifier(literal),
    }
}

pub struct Tokenizer<'a> {
    source_text: &'a str,
    source_chars: Peekable<Chars<'a>>,
    offset: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(sql: &'a str) -> Tokenizer<'a> {
        Tokenizer { source_text: sql, source_chars: sql.chars().peekable(), offset: 0 }
    }

    fn next_token(&mut self) -> Option<Token<'a>> {
        let mut start_offset = self.offset;
        while let Some(next_char) = self.source_chars.next() {
            self.offset += 1;
            match next_char {
                c if c.is_whitespace() => {
                    start_offset = self.offset;
                    continue;
                }

                // it can be a letter or an underscore
                c if c.is_alphabetic() || c == '\u{005f}' => {
                    while let Some(&next_sub_char) = self.source_chars.peek() {
                        // it can be a letter, a number or an underscore
                        if next_sub_char.is_alphanumeric() || c == '\u{005f}' {
                            self.source_chars.next();
                            self.offset += 1;
                            continue;
                        } else {
                            let literal = &self.source_text[start_offset..self.offset];
                            return Some(identify_keyword_or_identifier(literal));
                        }
                    }
                }

                c if c.is_numeric() => {
                    while let Some(&next_sub_char) = self.source_chars.peek() {
                        if next_sub_char.is_numeric() || next_sub_char == '.' {
                            self.source_chars.next();
                            self.offset += 1;
                            continue;
                        } else {
                            let literal = &self.source_text[start_offset..self.offset];
                            return Some(Token::NumericLiteral(literal));
                        }
                    }
                }

                '\'' => {
                    start_offset += 1; // skip the first quote
                    while let Some(&next_sub_char) = self.source_chars.peek() {
                        if next_sub_char == '\'' {
                            let literal = &self.source_text[start_offset..self.offset];
                            self.source_chars.next(); // consume the closing quote
                            self.offset += 1;
                            return Some(Token::StringLiteral(literal));
                        } else {
                            self.source_chars.next();
                            self.offset += 1;
                            continue;
                        }
                    }
                }

                // Operators
                ';' => return Some(Token::Semicolon),
                ',' => return Some(Token::Comma),
                '=' => return Some(Token::Equals),
                '(' => return Some(Token::LP),
                ')' => return Some(Token::RP),
                _ => return Some(Token::InvalidCharacter(next_char)),
            };
        }
        None
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::alloc::{GlobalAlloc, Layout, System};

    #[test]
    fn test_select_query() {
        let sql = "SELECT id FROM users WHERE age = 25 AND name = 'John';";
        let tokens: Vec<Token> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], Token::Select);
        assert_eq!(tokens[1], Token::Identifier("id"));
        assert_eq!(tokens[2], Token::From);
        assert_eq!(tokens[3], Token::Identifier("users"));
        assert_eq!(tokens[4], Token::Where);
        assert_eq!(tokens[5], Token::Identifier("age"));
        assert_eq!(tokens[6], Token::Equals);
        assert_eq!(tokens[7], Token::NumericLiteral("25"));
        assert_eq!(tokens[8], Token::And);
        assert_eq!(tokens[9], Token::Identifier("name"));
        assert_eq!(tokens[10], Token::Equals);
        assert_eq!(tokens[11], Token::StringLiteral("John"));
        assert_eq!(tokens[12], Token::Semicolon);
    }

    #[test]
    fn test_select_query_lowercase_keywords() {
        let sql = "select id from users where age = 25 and name = 'John';";
        let tokens: Vec<Token> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], Token::Select);
        assert_eq!(tokens[1], Token::Identifier("id"));
        assert_eq!(tokens[2], Token::From);
        assert_eq!(tokens[3], Token::Identifier("users"));
        assert_eq!(tokens[4], Token::Where);
        assert_eq!(tokens[5], Token::Identifier("age"));
        assert_eq!(tokens[6], Token::Equals);
        assert_eq!(tokens[7], Token::NumericLiteral("25"));
        assert_eq!(tokens[8], Token::And);
        assert_eq!(tokens[9], Token::Identifier("name"));
        assert_eq!(tokens[10], Token::Equals);
        assert_eq!(tokens[11], Token::StringLiteral("John"));
        assert_eq!(tokens[12], Token::Semicolon);
    }

    #[test]
    fn test_invalid_character() {
        let sql = "!";
        let tokens: Vec<Token> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], Token::InvalidCharacter('!'));
    }

    #[test]
    fn test_create_table() {
        let sql = "CREATE TABLE users (\
            id int,\
            name varchar(255)\
        );";
        let tokens: Vec<Token> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], Token::Create);
        assert_eq!(tokens[1], Token::Table);
        assert_eq!(tokens[2], Token::Identifier("users"));
        assert_eq!(tokens[3], Token::LP);
        assert_eq!(tokens[4], Token::Identifier("id"));
        assert_eq!(tokens[5], Token::Identifier("int"));
        assert_eq!(tokens[6], Token::Comma);
        assert_eq!(tokens[7], Token::Identifier("name"));
        assert_eq!(tokens[8], Token::Identifier("varchar"));
        assert_eq!(tokens[9], Token::LP);
        assert_eq!(tokens[10], Token::NumericLiteral("255"));
        assert_eq!(tokens[11], Token::RP);
        assert_eq!(tokens[12], Token::RP);
        assert_eq!(tokens[13], Token::Semicolon);
    }

    #[test]
    fn test_numeric_literal() {
        let sql = "SELECT 123.123;";
        let tokens: Vec<Token> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], Token::Select);
        assert_eq!(tokens[1], Token::NumericLiteral("123.123"));
        assert_eq!(tokens[2], Token::Semicolon);
    }

    #[test]
    fn test_no_alloc() {
        struct CountingAllocator;

        static ALLOCATION_COUNT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

        unsafe impl GlobalAlloc for CountingAllocator {
            unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
                ALLOCATION_COUNT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                System.alloc(layout)
            }

            unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
                System.dealloc(ptr, layout)
            }
        }

        #[global_allocator]
        static ALLOCATOR: CountingAllocator = CountingAllocator;

        let before = ALLOCATION_COUNT.load(std::sync::atomic::Ordering::SeqCst);
        let sql = "SELECT id FROM users WHERE age = 25;";
        let mut tokenizer = Tokenizer::new(sql);
        let _token = tokenizer.next();
        let after = ALLOCATION_COUNT.load(std::sync::atomic::Ordering::SeqCst);

        assert_eq!(before, after);
    }
}
