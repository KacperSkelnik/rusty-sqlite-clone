use crate::ParsingError::{
    InvalidStatement, MissingKeyword, MissingStringLiteral, StringLiteralExpected, UnexpectedEOF, UnexpectedKeyword,
    UnexpectedKeywordMany,
};
use crate::Statement::CreateTable;
use tokenizer::{Token, Tokenizer};

#[derive(Debug, PartialEq)]
pub enum ParsingError<'a> {
    InvalidStatement(Token<'a>),
    UnexpectedKeyword { expected: Token<'a>, actual: Token<'a> },
    UnexpectedKeywordMany { expected: Vec<Token<'a>>, actual: Token<'a> },
    MissingKeyword { expected: Token<'a> },
    StringLiteralExpected { actual: Token<'a> },
    MissingStringLiteral,
    UnexpectedError,
    UnexpectedEOF,
}

#[derive(Debug, PartialEq)]
enum Operator {
    Equals,
}

#[derive(Debug, PartialEq)]
struct ColumnDef<'a> {
    name: &'a str,
    data_type: &'a str,
}

#[derive(Debug, PartialEq)]
struct WhereClause<'a> {
    column: &'a str,
    operator: Operator,
    value: &'a str,
}

#[derive(Debug, PartialEq)]
enum Statement<'a> {
    CreateTable { columns: Vec<ColumnDef<'a>>, table: &'a str },
    Select { columns: Vec<&'a str>, table: &'a str, where_clause: Option<&'a str> },
    Insert { columns: Vec<&'a str>, table: &'a str, values: Vec<&'a str> },
}

struct Parser;

impl Parser {
    pub fn parse<'a>(&self, sql: &'a str) -> Result<Statement<'a>, ParsingError<'a>> {
        let mut tokens = Tokenizer::new(sql);

        if let Some(token) = tokens.next() {
            match token {
                Token::Create => Self.parse_create_statement(tokens),
                //Token::Select => Self.parse_select_statement(tokens),
                //Token::Insert => Self.parse_insert_statement(tokens),
                other => Err(InvalidStatement(other)),
            }
        } else {
            Err(UnexpectedEOF)
        }
    }

    fn parse_create_statement<'a>(&self, mut tokens: Tokenizer<'a>) -> Result<Statement<'a>, ParsingError<'a>> {
        // Expect "TABLE" keyword
        match tokens.next() {
            Some(Token::Table) => (),
            Some(other) => return Err(UnexpectedKeyword { expected: Token::Table, actual: other }),
            None => return Err(MissingKeyword { expected: Token::Table }),
        }

        // Parse table name
        let table = match tokens.next() {
            Some(Token::Identifier(name)) => name,
            Some(other) => return Err(StringLiteralExpected { actual: other }),
            None => return Err(MissingStringLiteral),
        };

        // Expect opening parenthesis
        match tokens.next() {
            Some(Token::LP) => (),
            Some(other) => return Err(UnexpectedKeyword { expected: Token::LP, actual: other }),
            None => return Err(MissingKeyword { expected: Token::LP }),
        }

        // Parse column list
        fn parse_column_def<'a>(
            columns_ref: &mut Vec<ColumnDef<'a>>,
            tokens_ref: &mut Tokenizer<'a>,
        ) -> Result<(), ParsingError<'a>> {
            let name = match tokens_ref.next() {
                Some(Token::Identifier(name)) => name,
                Some(other) => return Err(StringLiteralExpected { actual: other }),
                None => return Err(MissingStringLiteral),
            };
            let data_type = match tokens_ref.next() {
                Some(Token::Identifier(data_type)) => data_type,
                Some(other) => return Err(StringLiteralExpected { actual: other }),
                None => return Err(MissingStringLiteral),
            };
            columns_ref.push(ColumnDef { name, data_type });
            Ok(())
        }

        let mut columns: Vec<ColumnDef> = Vec::new();
        parse_column_def(&mut columns, &mut tokens)?;
        while let Some(next) = tokens.next() {
            match next {
                Token::RP => {
                    if columns.is_empty() {
                        return Err(UnexpectedEOF);
                    } else {
                        break;
                    }
                }
                Token::Comma => parse_column_def(&mut columns, &mut tokens)?,
                _ => return Err(UnexpectedKeywordMany { expected: vec![Token::RP, Token::Comma], actual: next }),
            }
        }

        if let Some(last_token) = tokens.next() {
            if last_token != Token::Semicolon {
                return Err(UnexpectedKeyword { actual: last_token, expected: Token::Semicolon });
            }
        } else {
            return Err(UnexpectedEOF);
        }

        Ok(CreateTable { columns, table })
    }

    fn parse_select_statement<'a>(&self, mut tokens: Tokenizer<'a>) -> Result<(), ParsingError<'a>> {
        Ok(())
    }

    fn parse_insert_statement<'a>(&self, mut tokens: Tokenizer<'a>) -> Result<(), ParsingError<'a>> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_table_success() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id INT, name TEXT, age INT)");
        assert_eq!(
            result,
            Ok(CreateTable {
                columns: vec![
                    ColumnDef { name: "id", data_type: "INT" },
                    ColumnDef { name: "name", data_type: "TEXT" },
                    ColumnDef { name: "age", data_type: "INT" }
                ],
                table: "users"
            })
        );
    }

    #[test]
    fn test_empty_input() {
        let parser = Parser {};
        let result = parser.parse("");
        assert_eq!(result, Err(UnexpectedEOF));
    }

    #[test]
    fn test_invalid_statement() {
        let parser = Parser {};
        let result = parser.parse("DROP TABLE users");
        assert_eq!(result, Err(InvalidStatement(Token::Identifier("DROP"))));
    }

    #[test]
    fn test_missing_table_name() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE");
        assert_eq!(result, Err(MissingStringLiteral));
    }

    #[test]
    fn test_missing_closing_parenthesis() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id INT");
        assert_eq!(result, Err(UnexpectedEOF));
    }
}
