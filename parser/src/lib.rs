use crate::ParsingError::{InvalidStatement, LiteralExpected, MissingKeyword, UnexpectedEOF, UnexpectedKeywordMany};
use crate::Statement::CreateTable;
use tokenizer::{Token, TokenPosition, TokenWithOffset, Tokenizer};

#[derive(Debug, PartialEq)]
pub enum ParsingError<'a> {
    InvalidStatement(Token<'a>),
    UnexpectedKeyword { expected: Token<'a>, position: TokenPosition },
    UnexpectedKeywordMany { expected: Vec<Token<'a>>, position: TokenPosition },
    MissingKeyword { expected: Token<'a>, position: TokenPosition },
    LiteralExpected { position: TokenPosition },
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

        if let Some(token_with_offset) = tokens.next() {
            match token_with_offset.token {
                Token::Create => Self.parse_create_statement(tokens),
                Token::Select => Self.parse_select_statement(tokens),
                Token::Insert => Self.parse_insert_statement(tokens),
                other => Err(InvalidStatement(other)),
            }
        } else {
            Err(UnexpectedEOF)
        }
    }

    fn parse_create_statement<'a>(&self, mut tokens: Tokenizer<'a>) -> Result<Statement<'a>, ParsingError<'a>> {
        // Expect "TABLE" keyword
        match tokens.next() {
            Some(TokenWithOffset { token: Token::Table, offset: _ }) => (),
            Some(other) => {
                return Err(MissingKeyword { expected: Token::Table, position: tokens.get_position(other.offset) })
            }
            None => return Err(UnexpectedEOF),
        }

        // Parse table name
        let table = match tokens.next() {
            Some(TokenWithOffset { token: Token::Identifier(name), offset: _ }) => name,
            Some(other) => return Err(LiteralExpected { position: tokens.get_position(other.offset) }),
            None => return Err(UnexpectedEOF),
        };

        // Expect "("
        match tokens.next() {
            Some(TokenWithOffset { token: Token::LP, offset: _ }) => (),
            Some(other) => {
                return Err(MissingKeyword { expected: Token::LP, position: tokens.get_position(other.offset) })
            }
            None => return Err(UnexpectedEOF),
        }

        // Parse column definitions
        fn parse_column_def<'a>(tokens_ref: &mut Tokenizer<'a>) -> Result<ColumnDef<'a>, ParsingError<'a>> {
            let name = match tokens_ref.next() {
                Some(TokenWithOffset { token: Token::Identifier(name), offset: _ }) => name,
                Some(other) => return Err(LiteralExpected { position: tokens_ref.get_position(other.offset) }),
                None => return Err(UnexpectedEOF),
            };
            let data_type = match tokens_ref.next() {
                Some(TokenWithOffset { token: Token::Identifier(data_type), offset: _ }) => data_type,
                Some(other) => return Err(LiteralExpected { position: tokens_ref.get_position(other.offset) }),
                None => return Err(UnexpectedEOF),
            };
            Ok(ColumnDef { name, data_type })
        }

        let mut columns: Vec<ColumnDef> = Vec::new();
        let first_column = parse_column_def(&mut tokens)?;
        columns.push(first_column);

        while let next = tokens.next() {
            match next {
                Some(TokenWithOffset { token: Token::RP, offset: _ }) => {
                    if columns.is_empty() {
                        return Err(UnexpectedEOF);
                    } else {
                        break;
                    }
                }
                Some(TokenWithOffset { token: Token::Comma, offset: _ }) => {
                    let next_column = parse_column_def(&mut tokens)?;
                    columns.push(next_column);
                }
                Some(other) => {
                    return Err(UnexpectedKeywordMany {
                        expected: vec![Token::RP, Token::Comma],
                        position: tokens.get_position(other.offset),
                    })
                }
                None => return Err(UnexpectedEOF),
            }
        }

        Ok(CreateTable { columns, table })
    }

    fn parse_select_statement<'a>(&self, mut tokens: Tokenizer<'a>) -> Result<Statement<'a>, ParsingError<'a>> {
        todo!()
    }

    fn parse_insert_statement<'a>(&self, mut tokens: Tokenizer<'a>) -> Result<Statement<'a>, ParsingError<'a>> {
        todo!()
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
    fn test_create_table_single_column() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id INT)");
        assert_eq!(
            result,
            Ok(CreateTable { columns: vec![ColumnDef { name: "id", data_type: "INT" }], table: "users" })
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
        assert_eq!(result, Err(UnexpectedEOF));
    }

    #[test]
    fn test_missing_closing_parenthesis() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id INT");
        assert_eq!(result, Err(UnexpectedEOF));
    }

    #[test]
    fn test_unexpected_definition_separator() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id TEXT; name TEXT; age INT)");
        assert_eq!(
            result,
            Err(UnexpectedKeywordMany {
                expected: vec![Token::RP, Token::Comma],
                position: TokenPosition { line: 1, character: 27 }
            })
        );
    }

    #[test]
    fn test_missing_opening_parenthesis() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users id INT)");
        assert_eq!(
            result,
            Err(MissingKeyword { expected: Token::LP, position: TokenPosition { line: 1, character: 19 } })
        );
    }

    #[test]
    fn test_missing_column_data_type() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id, name TEXT)");
        assert_eq!(result, Err(LiteralExpected { position: TokenPosition { line: 1, character: 22 } }));
    }

    #[test]
    fn test_trailing_comma() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id INT,)");
        assert_eq!(result, Err(LiteralExpected { position: TokenPosition { line: 1, character: 27 } }));
    }
}
