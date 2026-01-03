use crate::ParsingError::{
    IdentifierExpected, InvalidStatement, LiteralExpected, MissingKeyword, UnexpectedEOF, UnexpectedKeyword,
    UnexpectedKeywordMany,
};
use crate::Statement::CreateTable;
use tokenizer::{Token, TokenPosition, TokenWithOffset, Tokenizer};

#[derive(Debug, PartialEq)]
pub enum ParsingError<'a> {
    InvalidStatement(Token<'a>),
    UnexpectedKeyword { expected: Token<'a>, got: Token<'a>, position: TokenPosition },
    UnexpectedKeywordMany { expected: Vec<Token<'a>>, got: Token<'a>, position: TokenPosition },
    MissingKeyword { expected: Token<'a>, position: TokenPosition },
    LiteralExpected { position: TokenPosition },
    IdentifierExpected { position: TokenPosition },
    UnexpectedEOF,
}

#[derive(Debug, PartialEq)]
enum Operator {
    Equals,
    NotEquals,
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
    Select { columns: Vec<&'a str>, table: &'a str, where_clause: Option<WhereClause<'a>> },
    Insert { columns: Vec<&'a str>, table: &'a str, values: Vec<&'a str> },
}

struct Parser;

// Statics
impl Parser {
    fn consume_expected_keyword<'a>(
        expected_token: Token<'a>,
        tokens_ref: &mut Tokenizer<'a>,
    ) -> Result<(), ParsingError<'a>> {
        match tokens_ref.next() {
            Some(TokenWithOffset { token, offset: _ }) if token == expected_token => Ok(()),
            Some(other) => Err(UnexpectedKeyword {
                expected: expected_token,
                got: other.token,
                position: tokens_ref.get_position(other.offset),
            }),
            None => Err(MissingKeyword { expected: expected_token, position: tokens_ref.get_end_position() }),
        }
    }

    fn consume_identifier<'a>(tokens_ref: &mut Tokenizer<'a>) -> Result<&'a str, ParsingError<'a>> {
        match tokens_ref.next() {
            Some(TokenWithOffset { token: Token::Identifier(identifier), offset: _ }) => Ok(identifier),
            Some(other) => Err(IdentifierExpected { position: tokens_ref.get_position(other.offset) }),
            None => Err(IdentifierExpected { position: tokens_ref.get_end_position() }),
        }
    }

    fn parse_create_table_statement(mut tokens: Tokenizer) -> Result<Statement, ParsingError> {
        // Expect "TABLE" keyword
        Parser::consume_expected_keyword(Token::Table, &mut tokens)?;

        // Parse table name
        let table = Parser::consume_identifier(&mut tokens)?;

        // Expect "("
        Parser::consume_expected_keyword(Token::LP, &mut tokens)?;

        // Parse column definitions
        fn parse_column_def<'a>(tokens_ref: &mut Tokenizer<'a>) -> Result<ColumnDef<'a>, ParsingError<'a>> {
            let name = Parser::consume_identifier(tokens_ref)?;
            let data_type = Parser::consume_identifier(tokens_ref)?;
            Ok(ColumnDef { name, data_type })
        }

        let mut columns: Vec<ColumnDef> = Vec::new();

        let first_column = parse_column_def(&mut tokens)?;
        columns.push(first_column);

        loop {
            let Some(TokenWithOffset { token, offset }) = tokens.next() else { return Err(UnexpectedEOF) };
            match token {
                Token::Comma => {
                    let next_column = parse_column_def(&mut tokens)?;
                    columns.push(next_column);
                }
                Token::RP => {
                    if columns.is_empty() {
                        return Err(IdentifierExpected { position: tokens.get_position(offset) });
                    } else {
                        break;
                    }
                }
                other => {
                    return Err(UnexpectedKeywordMany {
                        expected: vec![Token::RP, Token::Comma],
                        got: other,
                        position: tokens.get_position(offset),
                    })
                }
            }
        }

        Ok(CreateTable { columns, table })
    }

    fn parse_select_statement(mut tokens: Tokenizer) -> Result<Statement, ParsingError> {
        let mut columns: Vec<&str> = Vec::new();
        loop {
            let Some(TokenWithOffset { token, offset }) = tokens.next() else { return Err(UnexpectedEOF) };
            match token {
                Token::Comma => continue,
                Token::Identifier(column) => columns.push(column),
                Token::From => break,
                other => {
                    return Err(UnexpectedKeywordMany {
                        expected: vec![Token::From, Token::Comma],
                        got: other,
                        position: tokens.get_position(offset),
                    })
                }
            }
        }
        let table = Parser::consume_identifier(&mut tokens)?;

        Ok(Statement::Select { columns, table, where_clause: None })
    }

    fn parse_insert_statement(mut tokens: Tokenizer) -> Result<Statement, ParsingError> {
        // Expect "INTO" keyword
        Parser::consume_expected_keyword(Token::Into, &mut tokens)?;

        // Parse table name
        let table = Parser::consume_identifier(&mut tokens)?;

        fn parse_values<'a>(tokens_ref: &mut Tokenizer<'a>) -> Result<Vec<&'a str>, ParsingError<'a>> {
            let mut values: Vec<&str> = Vec::new();

            // Opening parenthesis
            Parser::consume_expected_keyword(Token::LP, tokens_ref)?;

            loop {
                let Some(TokenWithOffset { token, offset }) = tokens_ref.next() else {
                    return Err(UnexpectedEOF);
                };
                match token {
                    Token::StringLiteral(value) => values.push(value),
                    Token::Comma => continue,
                    Token::RP => {
                        if values.is_empty() {
                            return Err(LiteralExpected { position: tokens_ref.get_position(offset) });
                        } else {
                            break;
                        }
                    }
                    other => {
                        return Err(UnexpectedKeywordMany {
                            expected: vec![Token::Comma, Token::RP],
                            got: other,
                            position: tokens_ref.get_position(offset),
                        })
                    }
                }
            }
            Ok(values)
        }

        match tokens.next() {
            Some(TokenWithOffset { token: Token::Values, offset: _ }) => {
                let values = parse_values(&mut tokens)?;
                Ok(Statement::Insert { columns: vec![], table, values })
            }
            Some(TokenWithOffset { token: Token::LP, offset: _ }) => {
                let mut columns: Vec<&str> = Vec::new();
                loop {
                    let Some(TokenWithOffset { token, offset }) = tokens.next() else { return Err(UnexpectedEOF) };
                    match token {
                        Token::Identifier(column) => columns.push(column),
                        Token::Comma => continue,
                        Token::RP => {
                            if columns.is_empty() {
                                return Err(UnexpectedKeyword {
                                    expected: Token::Identifier("column name"),
                                    got: Token::RP,
                                    position: tokens.get_position(offset),
                                });
                            } else {
                                break;
                            }
                        }
                        other => {
                            return Err(UnexpectedKeywordMany {
                                expected: vec![Token::Comma, Token::RP],
                                got: other,
                                position: tokens.get_position(offset),
                            })
                        }
                    }
                }

                Parser::consume_expected_keyword(Token::Values, &mut tokens)?;
                let values = parse_values(&mut tokens)?;
                Ok(Statement::Insert { columns, table, values })
            }
            Some(other) => Err(UnexpectedKeywordMany {
                expected: vec![Token::Values, Token::LP],
                got: other.token,
                position: tokens.get_position(other.offset),
            }),
            None => Err(UnexpectedEOF),
        }
    }
}

impl Parser {
    pub fn parse<'a>(&self, sql: &'a str) -> Result<Statement<'a>, ParsingError<'a>> {
        let mut tokens = Tokenizer::new(sql);

        if let Some(token_with_offset) = tokens.next() {
            match token_with_offset.token {
                Token::Create => Parser::parse_create_table_statement(tokens),
                Token::Select => Parser::parse_select_statement(tokens),
                Token::Insert => Parser::parse_insert_statement(tokens),
                other => Err(InvalidStatement(other)),
            }
        } else {
            Err(UnexpectedEOF)
        }
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
        assert_eq!(result, Err(IdentifierExpected { position: TokenPosition { line: 1, character: 12 } }));
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
                got: Token::Semicolon,
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
            Err(UnexpectedKeyword {
                expected: Token::LP,
                got: Token::Identifier("id"),
                position: TokenPosition { line: 1, character: 19 }
            })
        );
    }

    #[test]
    fn test_missing_column_data_type() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id, name TEXT)");
        assert_eq!(result, Err(IdentifierExpected { position: TokenPosition { line: 1, character: 22 } }));
    }

    #[test]
    fn test_trailing_comma() {
        let parser = Parser {};
        let result = parser.parse("CREATE TABLE users (id INT,)");
        assert_eq!(result, Err(IdentifierExpected { position: TokenPosition { line: 1, character: 27 } }));
    }

    #[test]
    fn test_create_table_missing_table_keyword() {
        let parser = Parser {};
        let result = parser.parse("CREATE users (id INT)");
        assert_eq!(
            result,
            Err(UnexpectedKeyword {
                expected: Token::Table,
                got: Token::Identifier("users"),
                position: TokenPosition { line: 1, character: 7 }
            })
        );
    }

    #[test]
    fn test_insert_with_columns() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users (id, name) VALUES ('1', 'John')");
        assert_eq!(
            result,
            Ok(Statement::Insert { columns: vec!["id", "name"], table: "users", values: vec!["1", "John"] })
        );
    }

    #[test]
    fn test_insert_without_columns() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users VALUES ('1', 'John', '25')");
        assert_eq!(result, Ok(Statement::Insert { columns: vec![], table: "users", values: vec!["1", "John", "25"] }));
    }

    #[test]
    fn test_insert_missing_values_keyword() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users (id, name)");
        assert_eq!(
            result,
            Err(MissingKeyword { expected: Token::Values, position: TokenPosition { line: 1, character: 28 } })
        );
    }

    #[test]
    fn test_insert_missing_table_name() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO");
        assert_eq!(result, Err(IdentifierExpected { position: TokenPosition { line: 1, character: 11 } }));
    }

    #[test]
    fn test_insert_missing_into_keyword() {
        let parser = Parser {};
        let result = parser.parse("INSERT users VALUES ('1', 'John')");
        assert_eq!(
            result,
            Err(UnexpectedKeyword {
                expected: Token::Into,
                got: Token::Identifier("users"),
                position: TokenPosition { line: 1, character: 7 }
            })
        );
    }

    #[test]
    fn test_insert_empty_column_list() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users () VALUES ('1', 'John')");
        assert_eq!(
            result,
            Err(UnexpectedKeyword {
                expected: Token::Identifier("column name"),
                got: Token::RP,
                position: TokenPosition { line: 1, character: 19 }
            })
        );
    }

    #[test]
    fn test_insert_empty_values_list() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users VALUES ()");
        assert_eq!(result, Err(LiteralExpected { position: TokenPosition { line: 1, character: 26 } }));
    }

    #[test]
    fn test_insert_missing_values_opening_parenthesis() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users VALUES '1', 'John')");
        assert_eq!(
            result,
            Err(UnexpectedKeyword {
                expected: Token::LP,
                got: Token::StringLiteral("1"),
                position: TokenPosition { line: 1, character: 25 }
            })
        );
    }

    #[test]
    fn test_insert_missing_columns_closing_parenthesis() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users (id, name VALUES ('1', 'John')");
        assert_eq!(
            result,
            Err(UnexpectedKeywordMany {
                expected: vec![Token::Comma, Token::RP],
                got: Token::Values,
                position: TokenPosition { line: 1, character: 28 }
            })
        );
    }

    #[test]
    fn test_insert_invalid_token_in_column_list() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users (id, 123) VALUES ('1', 'John')");
        assert_eq!(
            result,
            Err(UnexpectedKeywordMany {
                expected: vec![Token::Comma, Token::RP],
                got: Token::NumericLiteral("123"),
                position: TokenPosition { line: 1, character: 23 }
            })
        );
    }

    #[test]
    fn test_insert_invalid_token_in_values_list() {
        let parser = Parser {};
        let result = parser.parse("INSERT INTO users VALUES ('1', id)");
        assert_eq!(
            result,
            Err(UnexpectedKeywordMany {
                expected: vec![Token::Comma, Token::RP],
                got: Token::Identifier("id"),
                position: TokenPosition { line: 1, character: 31 }
            })
        );
    }

    #[test]
    fn test_select() {
        let parser = Parser {};
        let result = parser.parse("SELECT id, name FROM users");
        assert_eq!(result, Ok(Statement::Select { columns: vec!["id", "name"], table: "users", where_clause: None }));
    }
}
