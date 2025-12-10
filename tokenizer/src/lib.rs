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
    Insert,
    Into,
    Values,
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

#[derive(Debug, PartialEq)]
pub struct TokenWithOffset<'a> {
    pub token: Token<'a>,
    pub offset: usize,
}

#[derive(Debug, PartialEq)]
pub struct TokenPosition {
    pub line: usize,
    pub character: usize,
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
}

impl<'a> Tokenizer<'a> {
    fn identify_keyword_or_identifier(literal: &'a str) -> Token<'a> {
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
                "INSERT" => Token::Insert,
                "INTO" => Token::Into,
                "VALUES" => Token::Values,
                _ => Token::Identifier(literal),
            },
            Err(_) => Token::Identifier(literal),
        }
    }

    fn next_token(&mut self) -> Option<TokenWithOffset<'a>> {
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
                            let token = Self::identify_keyword_or_identifier(literal);
                            return Some(TokenWithOffset { token, offset: start_offset });
                        }
                    }
                    // we reached the end of the input without finding a terminating character
                    let literal = &self.source_text[start_offset..];
                    let token = Self::identify_keyword_or_identifier(literal);
                    return Some(TokenWithOffset { token, offset: start_offset });
                }

                c if c.is_numeric() => {
                    while let Some(&next_sub_char) = self.source_chars.peek() {
                        if next_sub_char.is_numeric() || next_sub_char == '.' {
                            self.source_chars.next();
                            self.offset += 1;
                            continue;
                        } else {
                            let literal = &self.source_text[start_offset..self.offset];
                            let token = Token::NumericLiteral(literal);
                            return Some(TokenWithOffset { token, offset: start_offset });
                        }
                    }
                    // we reached the end of the input without finding a terminating character
                    let literal = &self.source_text[start_offset..];
                    let token = Token::NumericLiteral(literal);
                    return Some(TokenWithOffset { token, offset: start_offset });
                }

                '\'' => {
                    start_offset += 1; // skip the first quote
                    while let Some(next_sub_char) = self.source_chars.next() {
                        if next_sub_char == '\'' {
                            let literal = &self.source_text[start_offset..self.offset];
                            self.offset += 1;
                            let token = Token::StringLiteral(literal);
                            return Some(TokenWithOffset { token, offset: start_offset - 1 });
                        } else {
                            self.offset += 1;
                            continue;
                        }
                    }
                }

                // Operators
                ';' => return Some(TokenWithOffset { token: Token::Semicolon, offset: start_offset }),
                ',' => return Some(TokenWithOffset { token: Token::Comma, offset: start_offset }),
                '=' => return Some(TokenWithOffset { token: Token::Equals, offset: start_offset }),
                '(' => return Some(TokenWithOffset { token: Token::LP, offset: start_offset }),
                ')' => return Some(TokenWithOffset { token: Token::RP, offset: start_offset }),
                _ => return Some(TokenWithOffset { token: Token::InvalidCharacter(next_char), offset: start_offset }),
            };
        }
        None
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = TokenWithOffset<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl<'a> Tokenizer<'a> {
    pub fn get_position(&self, offset: usize) -> TokenPosition {
        let mut line = 1;
        let mut character = 0;
        for (i, c) in self.source_text.chars().enumerate() {
            if i == offset {
                break;
            }
            if c == '\n' {
                line += 1;
                character = 0;
            } else {
                character += 1;
            }
        }
        TokenPosition { line, character }
    }

    pub fn get_end_position(&self) -> TokenPosition {
        self.get_position(self.source_text.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::alloc::{GlobalAlloc, Layout, System};

    #[test]
    fn test_select_query() {
        let sql = "SELECT id FROM users WHERE age = 25 AND name = 'John';";
        let tokens: Vec<TokenWithOffset> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], TokenWithOffset { token: Token::Select, offset: 0 });
        assert_eq!(tokens[1], TokenWithOffset { token: Token::Identifier("id"), offset: 7 });
        assert_eq!(tokens[2], TokenWithOffset { token: Token::From, offset: 10 });
        assert_eq!(tokens[3], TokenWithOffset { token: Token::Identifier("users"), offset: 15 });
        assert_eq!(tokens[4], TokenWithOffset { token: Token::Where, offset: 21 });
        assert_eq!(tokens[5], TokenWithOffset { token: Token::Identifier("age"), offset: 27 });
        assert_eq!(tokens[6], TokenWithOffset { token: Token::Equals, offset: 31 });
        assert_eq!(tokens[7], TokenWithOffset { token: Token::NumericLiteral("25"), offset: 33 });
        assert_eq!(tokens[8], TokenWithOffset { token: Token::And, offset: 36 });
        assert_eq!(tokens[9], TokenWithOffset { token: Token::Identifier("name"), offset: 40 });
        assert_eq!(tokens[10], TokenWithOffset { token: Token::Equals, offset: 45 });
        assert_eq!(tokens[11], TokenWithOffset { token: Token::StringLiteral("John"), offset: 47 });
        assert_eq!(tokens[12], TokenWithOffset { token: Token::Semicolon, offset: 53 });
    }

    #[test]
    fn test_select_query_lowercase_keywords() {
        let sql = "select id from users where age = 25 and name = 'John';";
        let tokens: Vec<TokenWithOffset> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], TokenWithOffset { token: Token::Select, offset: 0 });
        assert_eq!(tokens[1], TokenWithOffset { token: Token::Identifier("id"), offset: 7 });
        assert_eq!(tokens[2], TokenWithOffset { token: Token::From, offset: 10 });
        assert_eq!(tokens[3], TokenWithOffset { token: Token::Identifier("users"), offset: 15 });
        assert_eq!(tokens[4], TokenWithOffset { token: Token::Where, offset: 21 });
        assert_eq!(tokens[5], TokenWithOffset { token: Token::Identifier("age"), offset: 27 });
        assert_eq!(tokens[6], TokenWithOffset { token: Token::Equals, offset: 31 });
        assert_eq!(tokens[7], TokenWithOffset { token: Token::NumericLiteral("25"), offset: 33 });
        assert_eq!(tokens[8], TokenWithOffset { token: Token::And, offset: 36 });
        assert_eq!(tokens[9], TokenWithOffset { token: Token::Identifier("name"), offset: 40 });
        assert_eq!(tokens[10], TokenWithOffset { token: Token::Equals, offset: 45 });
        assert_eq!(tokens[11], TokenWithOffset { token: Token::StringLiteral("John"), offset: 47 });
        assert_eq!(tokens[12], TokenWithOffset { token: Token::Semicolon, offset: 53 });
    }

    #[test]
    fn test_invalid_character() {
        let sql = "!";
        let tokens: Vec<TokenWithOffset> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], TokenWithOffset { token: Token::InvalidCharacter('!'), offset: 0 });
    }

    #[test]
    fn test_create_table() {
        let sql = "CREATE TABLE users (\n
            id int,\n
            name varchar(255)\n
        );";
        let tokens: Vec<TokenWithOffset> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], TokenWithOffset { token: Token::Create, offset: 0 });
        assert_eq!(tokens[1], TokenWithOffset { token: Token::Table, offset: 7 });
        assert_eq!(tokens[2], TokenWithOffset { token: Token::Identifier("users"), offset: 13 });
        assert_eq!(tokens[3], TokenWithOffset { token: Token::LP, offset: 19 });
        assert_eq!(tokens[4], TokenWithOffset { token: Token::Identifier("id"), offset: 34 });
        assert_eq!(tokens[5], TokenWithOffset { token: Token::Identifier("int"), offset: 37 });
        assert_eq!(tokens[6], TokenWithOffset { token: Token::Comma, offset: 40 });
        assert_eq!(tokens[7], TokenWithOffset { token: Token::Identifier("name"), offset: 55 });
        assert_eq!(tokens[8], TokenWithOffset { token: Token::Identifier("varchar"), offset: 60 });
        assert_eq!(tokens[9], TokenWithOffset { token: Token::LP, offset: 67 });
        assert_eq!(tokens[10], TokenWithOffset { token: Token::NumericLiteral("255"), offset: 68 });
        assert_eq!(tokens[11], TokenWithOffset { token: Token::RP, offset: 71 });
        assert_eq!(tokens[12], TokenWithOffset { token: Token::RP, offset: 82 });
        assert_eq!(tokens[13], TokenWithOffset { token: Token::Semicolon, offset: 83 });
    }

    #[test]
    fn test_numeric_literal() {
        let sql = "SELECT 123.123;";
        let tokens: Vec<TokenWithOffset> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], TokenWithOffset { token: Token::Select, offset: 0 });
        assert_eq!(tokens[1], TokenWithOffset { token: Token::NumericLiteral("123.123"), offset: 7 });
        assert_eq!(tokens[2], TokenWithOffset { token: Token::Semicolon, offset: 14 });
    }

    #[test]
    fn test_insert_statement() {
        let sql = "INSERT INTO users VALUES ('John', 25);";
        let tokens: Vec<TokenWithOffset> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], TokenWithOffset { token: Token::Insert, offset: 0 });
        assert_eq!(tokens[1], TokenWithOffset { token: Token::Into, offset: 7 });
        assert_eq!(tokens[2], TokenWithOffset { token: Token::Identifier("users"), offset: 12 });
        assert_eq!(tokens[3], TokenWithOffset { token: Token::Values, offset: 18 });
        assert_eq!(tokens[4], TokenWithOffset { token: Token::LP, offset: 25 });
        assert_eq!(tokens[5], TokenWithOffset { token: Token::StringLiteral("John"), offset: 26 });
        assert_eq!(tokens[6], TokenWithOffset { token: Token::Comma, offset: 32 });
        assert_eq!(tokens[7], TokenWithOffset { token: Token::NumericLiteral("25"), offset: 34 });
        assert_eq!(tokens[8], TokenWithOffset { token: Token::RP, offset: 36 });
        assert_eq!(tokens[9], TokenWithOffset { token: Token::Semicolon, offset: 37 });
    }

    #[test]
    fn test_empty_input() {
        let sql = "";
        let tokens: Vec<TokenWithOffset> = Tokenizer::new(sql).collect();
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn test_incomplete_input() {
        let sql = "CREATE TABLE";
        let tokens: Vec<TokenWithOffset> = Tokenizer::new(sql).collect();
        assert_eq!(tokens[0], TokenWithOffset { token: Token::Create, offset: 0 });
        assert_eq!(tokens[1], TokenWithOffset { token: Token::Table, offset: 7 });
    }

    #[test]
    fn test_get_position() {
        let sql = "SELECT id\nFROM users\nWHERE age = 25;";
        let tokenizer = Tokenizer::new(sql);

        assert_eq!(tokenizer.get_position(0), TokenPosition { line: 1, character: 0 });
        assert_eq!(tokenizer.get_position(9), TokenPosition { line: 1, character: 9 });
        assert_eq!(tokenizer.get_position(10), TokenPosition { line: 2, character: 0 });
        assert_eq!(tokenizer.get_position(19), TokenPosition { line: 2, character: 9 });
        assert_eq!(tokenizer.get_position(20), TokenPosition { line: 2, character: 10 });
        assert_eq!(tokenizer.get_position(27), TokenPosition { line: 3, character: 6 });
    }

    #[test]
    fn test_get_position_single_line() {
        let sql = "SELECT id FROM users";
        let tokenizer = Tokenizer::new(sql);

        assert_eq!(tokenizer.get_position(7), TokenPosition { line: 1, character: 7 });
        assert_eq!(tokenizer.get_position(15), TokenPosition { line: 1, character: 15 });
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
