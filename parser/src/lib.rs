use crate::ParsingError::{EmptyInput, InvalidToken};
use tokenizer::{Token, Tokenizer};

pub enum ParsingError<'a> {
    EmptyInput,
    InvalidToken(Token<'a>),
}

enum Statement<'a> {
    CreateTable { columns: Vec<&'a str>, table: &'a str },
    Select { columns: Vec<&'a str>, table: &'a str, where_clause: Option<&'a str> },
}

struct Parser;

impl Parser {
    pub fn parse<'a>(&self, sql: &'a str) -> Result<(), ParsingError<'a>> {
        let mut tokens = Tokenizer::new(sql);

        if let Some(token) = tokens.next() {
            match token {
                Token::Create => Self.parse_create_statement(tokens),
                Token::Select => Self.parse_select_statement(tokens),
                Token::Insert => Self.parse_insert_statement(tokens),
                other => Err(InvalidToken(other)),
            }
        } else {
            Err(EmptyInput)
        }
    }

    fn parse_create_statement(&self, tokens: Tokenizer) -> Result<(), ParsingError> {
        Ok(())
    }

    fn parse_select_statement(&self, tokens: Tokenizer) -> Result<(), ParsingError> {
        Ok(())
    }

    fn parse_insert_statement(&self, tokens: Tokenizer) -> Result<(), ParsingError> {
        Ok(())
    }
}
