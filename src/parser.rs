use crate::{lexer::*, ptree::*};

/// An error that can occur during parsing.
#[derive(Debug)]
pub enum ParseError {
    /// The parser expected some [Token], but a different token was found.
    ExpectFailed {
        /// The expected [Token].
        expected: Token,

        /// The found (actual) [Token].
        found: Token,
    },

    /// The parser expected one of many [Token]s, but a different token was found.
    ExpectAnyFailed {
        /// The expected [Token].
        expected: Vec<Token>,

        /// The found (actual) [Token].
        found: Token,
    },

    /// A custom error message.
    Custom(&'static str),

    /// The parser ran out of [Token]s.
    OutOfTokens,

    /// Tokens were left over after parsing.
    TrailingToken(Token),

    /// The parser encountered an unrecognized [Token].
    UnrecognizedToken(UnrecognizedTokenError),
}

impl<'source, const LOOKAHEAD: usize> Lexer<'source, LOOKAHEAD> {
    fn expect(&mut self, expected: Token) -> Result<&'source str, ParseError> {
        let found = self.next();
        let found = match found {
            None => return Err(ParseError::OutOfTokens),
            Some(Err(e)) => return Err(ParseError::UnrecognizedToken(e)),
            Some(Ok(found)) => found,
        };
        if found != expected {
            return Err(ParseError::ExpectFailed { expected, found });
        }
        Ok(self.slice())
    }

    fn expect_any(&mut self, expected: &[Token]) -> Result<&'source str, ParseError> {
        let found = self.next();
        let found = match found {
            None => return Err(ParseError::OutOfTokens),
            Some(Err(e)) => return Err(ParseError::UnrecognizedToken(e)),
            Some(Ok(found)) => found,
        };
        for token in expected {
            if found == *token {
                return Ok(self.slice());
            }
        }
        Err(ParseError::ExpectAnyFailed {
            expected: expected.to_vec(),
            found,
        })
    }

    fn peek_matches(&self, matches: Token) -> bool {
        self.peek() == Some(matches)
    }

    fn peekn_matches(&self, n: usize, matches: Token) -> bool {
        self.peekn(n) == Some(matches)
    }

    fn peek_matches_any(&self, matches: &[Token]) -> bool {
        for token in matches {
            if self.peek() == Some(*token) {
                return true;
            }
        }
        false
    }

    fn peekn_matches_any(&self, n: usize, matches: &[Token]) -> bool {
        for token in matches {
            if self.peekn(n) == Some(*token) {
                return true;
            }
        }
        false
    }

    fn has_next(&self) -> bool {
        self.peek().is_some()
    }
}

/// Attempts to parse a numeric literal.
pub fn parse_number(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    let slice = lexer.expect(Token::Integer)?;
    Ok(Expr::I64(
        slice
            .parse::<i64>()
            .expect("lexer-valid number should always parse"),
    ))
}

/// Attempts to parse a string literal.
pub fn parse_string(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    let slice = lexer.expect(Token::String)?;
    let s = &slice[1..slice.len() - 1]; // TODO: string escapes
    Ok(Expr::String(s.to_owned()))
}

/// Attempts to parse a lowercase identifier.
pub fn parse_lident(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    let slice = lexer.expect(Token::LIdent)?;
    Ok(Expr::LIdent(slice.to_owned()))
}

/// Attempts to parse an uppercase identifier.
pub fn parse_uident(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    let slice = lexer.expect(Token::UIdent)?;
    Ok(Expr::UIdent(slice.to_owned()))
}

/// Attempts to parse an identifier.
pub fn parse_ident(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    if lexer.peek_matches(Token::LIdent) {
        return parse_lident(lexer);
    }
    if lexer.peek_matches(Token::UIdent) {
        return parse_uident(lexer);
    }
    Err(ParseError::Custom("expected identifier"))
}

/// Attempts to parse an atomic expression.
pub fn parse_atomic_expr(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    if lexer.peek_matches(Token::Integer) {
        return parse_number(lexer);
    }
    if lexer.peek_matches(Token::String) {
        return parse_string(lexer);
    }
    if lexer.peek_matches(Token::LIdent) {
        return parse_lident(lexer);
    }
    if lexer.peek_matches(Token::UIdent) {
        return parse_uident(lexer);
    }
    Err(ParseError::Custom("expected atomic expression"))
}

/// Attempts to parse an expression.
pub fn parse_expr(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    // Array literals

    // Atomic and suffix expressions
    let expr = parse_atomic_expr(lexer)?;

    // Constructors
    if let Expr::UIdent(_) = &expr
        && lexer.peek_matches(Token::LCurl)
    {
        let Expr::UIdent(ident) = expr else {
            unreachable!()
        };
        lexer.expect(Token::LCurl)?;
        let args = parse_list(lexer, parse_expr)?;
        lexer.expect(Token::RCurl)?;
        return Ok(Expr::Constructor(Constructor { ident, args }));
    }

    // Function calls
    if lexer.peek_matches(Token::LParen) {
        lexer.expect(Token::LParen)?;
        let args = parse_list(lexer, parse_arg)?;
        lexer.expect(Token::RParen)?;
        return Ok(Expr::Call(Call {
            func: Box::new(expr),
            args,
        }));
    }

    Ok(expr)
}

/// Attempts to parse an entire program, emits an error on trailing tokens.
pub fn parse_program(lexer: &mut Lexer<'_, 2>) -> Result<Vec<Expr>, ParseError> {
    let program = parse_list(lexer, parse_expr)?;
    match lexer.next() {
        Some(Ok(token)) => Err(ParseError::TrailingToken(token)),
        Some(Err(e)) => Err(ParseError::UnrecognizedToken(e)),
        None => Ok(program),
    }
}

/// Attempts to parse a comma/newline-separated list with the given parser.
pub fn parse_list<T>(
    lexer: &mut Lexer<'_, 2>,
    parser: fn(&mut Lexer<'_, 2>) -> Result<T, ParseError>,
) -> Result<Vec<T>, ParseError> {
    let mut result = Vec::new();
    let mut first = true;
    while lexer.has_next()
        && !lexer.peek_matches_any(&[Token::RParen, Token::RSquare, Token::RCurl])
    {
        if !first {
            lexer.expect_any(&[Token::Comma, Token::Newline, Token::Semicolon])?;
        }
        while lexer.peek_matches(Token::Newline) {
            lexer.next();
        }
        if !lexer.has_next()
            || lexer.peek_matches_any(&[Token::RParen, Token::RSquare, Token::RCurl])
        {
            break;
        };
        result.push(parser(lexer)?);
        first = false;
    }
    Ok(result)
}

/// Attempts to parse a function argument.
pub fn parse_arg(lexer: &mut Lexer<'_, 2>) -> Result<Arg, ParseError> {
    let ident = if lexer.peek_matches(Token::LIdent) && lexer.peekn_matches(1, Token::Colon) {
        lexer.expect(Token::LIdent)?;
        let ident = lexer.slice().to_owned();
        lexer.expect(Token::Colon)?;
        Some(ident)
    } else {
        None
    };
    let is_mut = if lexer.peek_matches(Token::Ampersand) {
        lexer.next();
        true
    } else {
        false
    };
    Ok(Arg {
        ident,
        is_mut,
        expr: parse_expr(lexer)?,
    })
}
