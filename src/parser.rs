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
    /// Consumes the next [Token] and returns its slice, otherwise returns an error if it is
    /// different from the expected token.
    pub fn expect(&mut self, expected: Token) -> Result<&'source str, ParseError> {
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

    /// Consumes the next [Token] and returns its slice, otherwise returns an error if it is
    /// not equal to any of the expected tokens.
    pub fn expect_any(&mut self, expected: &[Token]) -> Result<&'source str, ParseError> {
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

    /// Returns [true] if the next token matches the given one, without consuming the token.
    pub fn peek_matches(&self, matches: Token) -> bool {
        self.peek() == Some(matches)
    }

    /// Returns [true] if the nth next token matches the given one, without consuming the token.
    pub fn peekn_matches(&self, n: usize, matches: Token) -> bool {
        self.peekn(n) == Some(matches)
    }

    /// Returns [true] if the next token matches any of the given ones, without consuming the token.
    pub fn peek_matches_any(&self, matches: &[Token]) -> bool {
        for token in matches {
            if self.peek() == Some(*token) {
                return true;
            }
        }
        false
    }

    /// Returns [true] if the nth next token matches any of the given ones, without consuming the token.
    pub fn peekn_matches_any(&self, n: usize, matches: &[Token]) -> bool {
        for token in matches {
            if self.peekn(n) == Some(*token) {
                return true;
            }
        }
        false
    }

    /// Returns [true] if the lexer has a next token, or [false] if we are out of tokens.
    pub fn has_next(&self) -> bool {
        self.peek().is_some()
    }
}

impl InfixKind {
    /// Returns the token associated with this infix operator.
    pub const fn tok(self) -> Token {
        match self {
            InfixKind::Add => Token::Plus,
            InfixKind::Sub => Token::Minus,
            InfixKind::Mul => Token::Times,
            InfixKind::Div => Token::Divide,
        }
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

/// Attempts to parse a single-token expression (e.g. identifiers, numbers, strings).
pub fn parse_single_token_expr(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
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

/// Attempts to parse an atomic expression (e.g. constructors, function calls).
pub fn parse_atomic_expr(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    let mut expr = parse_single_token_expr(lexer)?;

    loop {
        // Projections
        if lexer.peek_matches(Token::Dot) {
            lexer.expect(Token::Dot)?;
            let field = lexer
                .expect_any(&[Token::LIdent, Token::UIdent])?
                .to_owned();
            expr = Expr::Proj(Proj {
                object: Box::new(expr),
                field,
            });
            continue;
        }

        // Constructors
        if lexer.peek_matches(Token::LCurl) {
            lexer.expect(Token::LCurl)?;
            let args = parse_list(lexer, parse_atomic_expr)?;
            lexer.expect(Token::RCurl)?;
            expr = Expr::Constructor(Constructor {
                ty: Box::new(expr),
                args,
            });
            continue;
        }

        // Function calls
        if lexer.peek_matches(Token::LParen) {
            lexer.expect(Token::LParen)?;
            let args = parse_list(lexer, parse_arg)?;
            lexer.expect(Token::RParen)?;
            expr = Expr::Call(Call {
                func: Box::new(expr),
                args,
            });
            continue;
        }

        // Fallthrough
        return Ok(expr);
    }
}

/// Attempts to parse a right-recursive binary operator expression with the given follow-up parser.
pub fn parse_right_recursive_expr(
    lexer: &mut Lexer<'_, 2>,
    kind: InfixKind,
    parser: fn(&mut Lexer<'_, 2>) -> Result<Expr, ParseError>,
) -> Result<Expr, ParseError> {
    let expr = parser(lexer)?;
    if lexer.peek_matches(kind.tok()) {
        lexer.expect(kind.tok())?;
        return Ok(Expr::Infix(Infix {
            kind,
            lhs: Box::new(expr),
            rhs: Box::new(parse_right_recursive_expr(lexer, kind, parser)?),
        }));
    }
    Ok(expr)
}

/// Attempts to parse a left-recursive binary operator expression with the given follow-up parser.
pub fn parse_left_recursive_expr(
    lexer: &mut Lexer<'_, 2>,
    kind: InfixKind,
    parser: fn(&mut Lexer<'_, 2>) -> Result<Expr, ParseError>,
) -> Result<Expr, ParseError> {
    let mut expr = parser(lexer)?;
    while lexer.peek_matches(kind.tok()) {
        lexer.expect(kind.tok())?;
        expr = Expr::Infix(Infix {
            kind,
            lhs: Box::new(expr),
            rhs: Box::new(parser(lexer)?),
        });
    }
    Ok(expr)
}

/// Attempts to parse an expression.
pub fn parse_expr(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    parse_left_recursive_expr(lexer, InfixKind::Add, parse_atomic_expr)
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
        expr: parse_atomic_expr(lexer)?,
    })
}
