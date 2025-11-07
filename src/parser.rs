use std::num::{ParseFloatError, ParseIntError};

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
    Custom {
        /// The error message.
        message: &'static str,

        /// The token at which the error occured.
        at_token: Option<Token>,
    },

    /// The parser ran out of [Token]s.
    OutOfTokens,

    /// Tokens were left over after parsing.
    TrailingToken(Token),

    /// The parser encountered an unrecognized [Token].
    UnrecognizedToken(UnrecognizedTokenError),

    /// Two or more lengths were provided to a vector expression.
    IllegalVector {
        /// The number of count arguments provided to the vector.
        num_count_arguments: usize,
    },

    /// Failed to parse an integer.
    ParseIntError(ParseIntError),

    /// Failed to parse a float.
    ParseFloatError(ParseFloatError),
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
        Ok(self.slice().unwrap())
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
                return Ok(self.slice().unwrap());
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

/// A group of pratt-parsed operators of equal precedence with a given operator associativity.
pub struct PrattGroup<'a> {
    kinds: &'a [InfixKind],
    assoc: Assoc,
}

impl<'a> PrattGroup<'a> {
    /// Constructs a new left-associative pratt parsing group.
    pub fn left(kinds: &'a [InfixKind]) -> Self {
        Self {
            kinds,
            assoc: Assoc::Left,
        }
    }

    /// Constructs a new right-associative pratt parsing group.
    pub fn right(kinds: &'a [InfixKind]) -> Self {
        Self {
            kinds,
            assoc: Assoc::Right,
        }
    }
}

/// Infix operator associativity.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Assoc {
    /// Left-associative (e.g. `x $ y $ z` is parsed as `(x $ y) $ z`).
    Left,

    /// Right-associative (e.g. `x $ y $ z` is parsed as `x $ (y $ z)`).
    Right,
}

impl InfixKind {
    /// Returns the token associated with this infix operator.
    pub const fn tok(self) -> Token {
        match self {
            InfixKind::Add => Token::Plus,
            InfixKind::Sub => Token::Minus,
            InfixKind::Mul => Token::Times,
            InfixKind::Div => Token::Divide,
            InfixKind::Eq => Token::EqualsEquals,
            InfixKind::Ne => Token::NotEquals,
        }
    }
}

/// Attempts to parse a numeric literal.
pub fn parse_number(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    if lexer.peek_matches(Token::Integer) {
        let slice = lexer.expect(Token::Integer)?;
        return Ok(Expr::Integer(slice.to_owned()));
    }
    if lexer.peek_matches(Token::Float) {
        let slice = lexer.expect(Token::Float)?;
        return Ok(Expr::Float(slice.to_owned()));
    }

    Err(ParseError::ExpectAnyFailed {
        expected: vec![Token::Integer, Token::Float],
        found: lexer.peek().ok_or(ParseError::OutOfTokens)?,
    })
}

/// Attempts to parse a string literal.
pub fn parse_string(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    let slice = lexer.expect(Token::String)?;
    let s = &slice[1..slice.len() - 1]; // TODO: string escapes
    Ok(Expr::String(s.to_owned()))
}

/// Attempts to parse an identifier.
pub fn parse_ident(lexer: &mut Lexer<'_, 2>) -> Result<Ident, ParseError> {
    if lexer.peek_matches(Token::Underscore) {
        let slice = lexer.expect(Token::Underscore)?;
        return Ok(Ident {
            name: slice.to_owned(),
            kind: IdentKind::Void,
        });
    }
    if lexer.peek_matches(Token::LIdent) {
        let slice = lexer.expect(Token::LIdent)?;
        return Ok(Ident {
            name: slice.to_owned(),
            kind: IdentKind::Value,
        });
    }
    if lexer.peek_matches(Token::UIdent) {
        let slice = lexer.expect(Token::UIdent)?;
        return Ok(Ident {
            name: slice.to_owned(),
            kind: IdentKind::Type,
        });
    }
    if lexer.peek_matches(Token::LBIdent) {
        let slice = lexer.expect(Token::LBIdent)?;
        return Ok(Ident {
            name: slice.to_owned(),
            kind: IdentKind::BuiltinValue,
        });
    }
    if lexer.peek_matches(Token::UBIdent) {
        let slice = lexer.expect(Token::UBIdent)?;
        return Ok(Ident {
            name: slice.to_owned(),
            kind: IdentKind::BuiltinType,
        });
    }
    Err(ParseError::Custom {
        message: "expected identifier",
        at_token: lexer.peek(),
    })
}

/// Attempts to parse an array literal or a vector expression.
pub fn parse_array_or_vec(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    lexer.expect(Token::LSquare)?;
    let exprs = parse_list(lexer, parse_expr)?;
    lexer.expect(Token::RSquare)?;
    match parse_atomic_expression(lexer) {
        Ok(expr) => match exprs.len() {
            0 => Ok(Expr::Vector(Vector {
                count: None,
                expr: Box::new(expr),
            })),
            1 => Ok(Expr::Vector(Vector {
                count: Some(Box::new(exprs.into_iter().next().unwrap())),
                expr: Box::new(expr),
            })),
            n => Err(ParseError::IllegalVector {
                num_count_arguments: n,
            }),
        },
        Err(_) => Ok(Expr::Array(Array { exprs })),
    }
}

/// Attempts to parse an atomic expression (e.g. identifiers, numbers, strings, array literals).
pub fn parse_atomic_expression(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    if lexer.peek_matches(Token::LParen) {
        // TODO: this currently doesn't support newline-separated tuples
        lexer.expect(Token::LParen)?;
        while lexer.peek_matches(Token::Newline) {
            lexer.expect(Token::Newline)?;
        }
        let expr = parse_expr(lexer)?;
        while lexer.peek_matches(Token::Newline) {
            lexer.expect(Token::Newline)?;
        }
        lexer.expect(Token::RParen)?;
        return Ok(Expr::Paren(Paren {
            expr: Box::new(expr),
        }));
    }
    if lexer.peek_matches_any(&[
        Token::Underscore,
        Token::LIdent,
        Token::UIdent,
        Token::LBIdent,
        Token::UBIdent,
    ]) {
        return Ok(Expr::Ident(parse_ident(lexer)?));
    }
    if lexer.peek_matches(Token::LParen) {
        return Ok(Expr::Object(parse_object(lexer)?));
    }
    if lexer.peek_matches(Token::LCurl) {
        return Ok(Expr::Block(parse_block(lexer)?));
    }
    if lexer.peek_matches(Token::DollarSign) {
        return parse_return(lexer);
    }
    if lexer.peek_matches(Token::QuestionMark) {
        return parse_if(lexer);
    }
    if lexer.peek_matches(Token::LSquare) {
        return parse_array_or_vec(lexer);
    }
    if lexer.peek_matches_any(&[Token::Integer, Token::Float]) {
        return parse_number(lexer);
    }
    if lexer.peek_matches(Token::String) {
        return parse_string(lexer);
    }
    if !lexer.has_next() {
        return Err(ParseError::OutOfTokens);
    }
    Err(ParseError::Custom {
        message: "expected atomic expression",
        at_token: lexer.peek(),
    })
}

/// Attempts to parse a suffix expression (e.g. constructors, function calls).
pub fn parse_suffix_expr(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    let mut expr = parse_atomic_expression(lexer)?;

    loop {
        // Projections
        if lexer.peek_matches(Token::Dot) {
            lexer.expect(Token::Dot)?;
            let ident = parse_ident(lexer)?;
            expr = Expr::Proj(Proj {
                object: Box::new(expr),
                ident,
            });
            continue;
        }

        // Constructors
        // if lexer.peek_matches(Token::LCurl) {
        //     lexer.expect(Token::LCurl)?;
        //     let args = parse_list(lexer, parse_expr)?;
        //     lexer.expect(Token::RCurl)?;
        //     expr = Expr::Constructor(Constructor {
        //         ty: Box::new(expr),
        //         args,
        //     });
        //     continue;
        // }

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
/// The provided infix operator kinds are treated with equal precedence, higher-precedence
/// operators should belong to the passed in continuation.
pub fn parse_right_recursive_expr(
    lexer: &mut Lexer<'_, 2>,
    kinds: &[InfixKind],
    parser: impl Fn(&mut Lexer<'_, 2>) -> Result<Expr, ParseError>,
) -> Result<Expr, ParseError> {
    let expr = parser(lexer)?;
    for kind in kinds {
        if lexer.peek_matches(kind.tok()) {
            lexer.expect(kind.tok())?;
            return Ok(Expr::Infix(Infix {
                kind: *kind,
                lhs: Box::new(expr),
                rhs: Box::new(parse_right_recursive_expr(lexer, kinds, parser)?),
            }));
        }
    }
    Ok(expr)
}

/// Attempts to parse a left-recursive binary operator expression with the given follow-up parser.
/// The provided infix operator kinds are treated with equal precedence, higher-precedence
/// operators should belong to the passed in continuation.
pub fn parse_left_recursive_expr(
    lexer: &mut Lexer<'_, 2>,
    kinds: &[InfixKind],
    parser: impl Fn(&mut Lexer<'_, 2>) -> Result<Expr, ParseError>,
) -> Result<Expr, ParseError> {
    let mut expr = parser(lexer)?;
    loop {
        let mut matched = false;
        for kind in kinds {
            if lexer.peek_matches(kind.tok()) {
                lexer.expect(kind.tok())?;
                expr = Expr::Infix(Infix {
                    kind: *kind,
                    lhs: Box::new(expr),
                    rhs: Box::new(parser(lexer)?),
                });
                matched = true;
                break;
            }
        }
        if !matched {
            break;
        }
    }
    Ok(expr)
}

/// Parses a group of operators according to pratt parsing rules. Pratt groups provided in the
/// `groups` slice are of increasing precedence. Operators _within_ the same group are of equal
/// precedence.
pub fn parse_pratt(lexer: &mut Lexer<'_, 2>, groups: &[PrattGroup]) -> Result<Expr, ParseError> {
    match groups {
        [] => parse_suffix_expr(lexer),
        _ => {
            let group = &groups[0];
            match group.assoc {
                Assoc::Left => parse_left_recursive_expr(lexer, group.kinds, |lexer| {
                    parse_pratt(lexer, &groups[1..])
                }),
                Assoc::Right => parse_right_recursive_expr(lexer, group.kinds, |lexer| {
                    parse_pratt(lexer, &groups[1..])
                }),
            }
        }
    }
}

/// Attempts to parse an expression.
pub fn parse_expr(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    parse_pratt(
        lexer,
        &[
            PrattGroup::left(&[InfixKind::Add, InfixKind::Sub]),
            PrattGroup::left(&[InfixKind::Mul, InfixKind::Div]),
            PrattGroup::left(&[InfixKind::Eq, InfixKind::Ne]),
        ],
    )
}

/// Attempts to parse an entire program, emits an error on trailing tokens.
pub fn parse_program(lexer: &mut Lexer<'_, 2>) -> Result<Vec<Field>, ParseError> {
    let program = parse_list(lexer, parse_field)?;
    if lexer.peek_matches(Token::Newline) {
        lexer.expect(Token::Newline)?;
    }
    match lexer.next() {
        Some(Ok(token)) => Err(ParseError::TrailingToken(token)),
        Some(Err(e)) => Err(ParseError::UnrecognizedToken(e)),
        None => Ok(program),
    }
}

/// Attempts to parse a comma/newline-separated list with the given parser.
pub fn parse_list<T>(
    lexer: &mut Lexer<'_, 2>,
    parser: impl Fn(&mut Lexer<'_, 2>) -> Result<T, ParseError>,
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
        let ident = parse_ident(lexer)?;
        lexer.expect(Token::Colon)?;
        Some(ident)
    } else {
        None
    };
    let is_mut = if lexer.peek_matches(Token::Times) {
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

/// Attempts to parse a declaration statement.
pub fn parse_decl(lexer: &mut Lexer<'_, 2>) -> Result<Stmt, ParseError> {
    let ident = parse_ident(lexer)?;
    lexer.expect(Token::Equals)?;
    let expr = parse_expr(lexer)?;
    Ok(Stmt::Decl(ident, expr))
}

/// Attempts to parse an assignment statement.
pub fn parse_assn(lexer: &mut Lexer<'_, 2>) -> Result<Stmt, ParseError> {
    let ident = parse_ident(lexer)?;
    lexer.expect(Token::ColonEquals)?;
    let expr = parse_expr(lexer)?;
    Ok(Stmt::Assn(ident, expr))
}

/// Attempts to parse a return statement.
pub fn parse_return(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    lexer.expect(Token::DollarSign)?;
    let expr = Box::new(parse_expr(lexer)?);
    Ok(Expr::Return(Return { expr }))
}

/// Attempts to parse a statement.
pub fn parse_stmt(lexer: &mut Lexer<'_, 2>) -> Result<Stmt, ParseError> {
    // Declarations
    if lexer.peekn_matches(1, Token::Equals) {
        return parse_decl(lexer);
    }

    // Assignments
    if lexer.peekn_matches(1, Token::ColonEquals) {
        return parse_assn(lexer);
    }

    Ok(Stmt::Expr(parse_expr(lexer)?))
}

/// Attempts to parse an if expression.
pub fn parse_if(lexer: &mut Lexer<'_, 2>) -> Result<Expr, ParseError> {
    lexer.expect(Token::QuestionMark)?;
    lexer.expect(Token::LParen)?;
    let cond = Box::new(parse_expr(lexer)?);
    lexer.expect(Token::RParen)?;
    let if_body = Box::new(parse_expr(lexer)?);
    let else_body = if lexer.peek_matches(Token::Colon) {
        lexer.expect(Token::Colon)?;
        Some(Box::new(parse_expr(lexer)?))
    } else {
        None
    };

    Ok(Expr::If(If {
        cond,
        if_body,
        else_body,
    }))
}

/// Attempts to parse an object.
pub fn parse_object(lexer: &mut Lexer<'_, 2>) -> Result<Object, ParseError> {
    lexer.expect(Token::LParen)?;
    let fields = parse_list(lexer, parse_field)?;
    lexer.expect(Token::RParen)?;
    Ok(Object { fields })
}

/// Attempts to parse a field.
pub fn parse_field(lexer: &mut Lexer<'_, 2>) -> Result<Field, ParseError> {
    let decorator = if lexer.peek_matches(Token::Times) {
        lexer.expect(Token::Times)?;
        Decorator::Mutable
    } else {
        Decorator::Default
    };
    let ident = parse_ident(lexer)?;

    // Object/function syntax sugar
    if lexer.peek_matches(Token::LParen) {
        let obj = parse_object(lexer)?;

        // Object
        if !lexer.peek_matches_any(&[Token::LCurl, Token::RightArrow]) {
            return Ok(Field {
                decorator,
                ident,
                ty: Expr::Object(obj),
                default_value: None,
            });
        }

        if lexer.peek_matches(Token::LCurl) {
            let body = parse_block(lexer)?;
            return Ok(Field {
                decorator,
                ident,
                ty: Expr::Func(Func {
                    params: obj,
                    ty: None,
                    body: Some(body),
                }),
                default_value: None,
            });
        } else if lexer.peek_matches(Token::RightArrow) {
            lexer.expect(Token::RightArrow)?;
            let ty = parse_expr(lexer)?;
            let body = if lexer.peek_matches(Token::LCurl) {
                Some(parse_block(lexer)?)
            } else {
                None
            };
            return Ok(Field {
                decorator,
                ident,
                ty: Expr::Func(Func {
                    params: obj,
                    ty: Some(Box::new(ty)),
                    body,
                }),
                default_value: None,
            });
        } else {
            return Ok(Field {
                decorator,
                ident,
                ty: Expr::Object(obj),
                default_value: None,
            });
        }
    } else {
        lexer.expect_any(&[Token::LParen, Token::Colon])?;
    }

    // Normal field
    let ty = parse_expr(lexer)?;
    let default_value = if lexer.peek_matches(Token::Equals) {
        lexer.expect(Token::Equals)?;
        Some(parse_expr(lexer)?)
    } else {
        None
    };
    Ok(Field {
        decorator,
        ident,
        ty,
        default_value,
    })
}

/// Attempts to parse a function parameter.
pub fn parse_param(lexer: &mut Lexer<'_, 2>) -> Result<Param, ParseError> {
    let ident = parse_ident(lexer)?;
    lexer.expect(Token::Colon)?;
    let ty = parse_expr(lexer)?;
    Ok(Param {
        is_mut: false,
        ident,
        ty,
    })
}

/// Attempts to parse a code block.
pub fn parse_block(lexer: &mut Lexer<'_, 2>) -> Result<Block, ParseError> {
    lexer.expect(Token::LCurl)?;
    let stmts = parse_list(lexer, parse_stmt)?;
    lexer.expect(Token::RCurl)?;
    Ok(Block { stmts })
}
