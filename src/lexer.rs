use logos::Logos;

#[allow(missing_docs)]
#[derive(Logos, Clone, Copy, Debug, PartialEq)]
#[logos(skip r"[ \t]+")]
pub enum Token {
    #[regex(r"[\n\f\r]+")]
    Newline,

    #[token(r",")]
    Comma,

    #[token(r"=")]
    Equals,

    #[token(r":=")]
    ColonEquals,

    #[token(r"==")]
    EqualsEquals,

    #[token(r"!=")]
    NotEquals,

    #[token(r"..")]
    DotDot,

    #[token(r".")]
    Dot,

    #[token(r":")]
    Colon,

    #[token(r"$")]
    DollarSign,

    #[token(r"::")]
    ColonColon,

    #[token(r";")]
    Semicolon,

    #[token(r"&")]
    Ampersand,

    #[token(r"?")]
    QuestionMark,

    #[token(r"<-")]
    LeftArrow,

    #[token(r"->")]
    RightArrow,

    #[token(r"|")]
    Bar,

    #[token(r"+")]
    Plus,

    #[token(r"++")]
    PlusPlus,

    #[token(r"-")]
    Minus,

    #[token(r"*")]
    Times,

    #[token(r"**")]
    TimesTimes,

    #[token(r"/")]
    Divide,

    #[token(r"//")]
    DivideDivide,

    #[token(r"<<")]
    LShift,

    #[token(r">>")]
    RShift,

    #[token(r"%")]
    Percent,

    #[token(r"(")]
    LParen,

    #[token(r")")]
    RParen,

    #[token(r"{")]
    LCurl,

    #[token(r"}")]
    RCurl,

    #[token(r"[")]
    LSquare,

    #[token(r"]")]
    RSquare,

    #[token(r"<")]
    LAngle,

    #[token(r">")]
    RAngle,

    #[regex(r"[\d]+")]
    Integer,

    #[regex(r"[\d]+\.[\d]+")]
    Float,

    #[regex(r"_")]
    Underscore,

    #[regex(r"[A-Z][a-zA-Z0-9]*")]
    UIdent,

    #[regex(r"[a-z][_a-z0-9]*")]
    LIdent,

    #[regex(r#"["]([^"\\\n]|\\.|\\\n)*["]"#)]
    String,

    #[regex(r#"[']([^'\\\n]|\\.|\\\n)*[']"#)]
    Character,
}

/// A lexer wrapping the [logos::Lexer] type.
pub struct Lexer<'source, const LOOKAHEAD: usize> {
    lexer: logos::Lexer<'source, Token>,
    next: [Option<Token>; LOOKAHEAD],
    slice: [Option<&'source str>; LOOKAHEAD],
}

/// An unrecognized token was encountered.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UnrecognizedTokenError;

impl<'source, const LOOKAHEAD: usize> Lexer<'source, LOOKAHEAD> {
    /// Constructs a new [Lexer] from the given source code.
    pub fn new(source: &'source str) -> Result<Self, UnrecognizedTokenError> {
        let mut lexer = Token::lexer(source);

        let mut next = [None; LOOKAHEAD];
        let mut slice = [None; LOOKAHEAD];
        for i in 0..LOOKAHEAD {
            slice[i] = Some(lexer.slice());
            next[i] = match lexer.next() {
                Some(Ok(next)) => Some(next),
                Some(Err(_)) => return Err(UnrecognizedTokenError),
                None => None,
            };
        }

        Ok(Self { next, slice, lexer })
    }

    /// Returns the source code's string slice.
    pub fn slice(&self) -> Option<&'source str> {
        self.slice.first().copied().flatten()
    }

    /// Peeks the next [Token] without consuming it.
    pub fn peek(&self) -> Option<Token> {
        self.next[0]
    }

    /// Peeks the nth next [Token] without consuming it.
    pub fn peekn(&self, n: usize) -> Option<Token> {
        self.next[n]
    }
}

impl<const LOOKAHEAD: usize> Iterator for Lexer<'_, LOOKAHEAD> {
    type Item = Result<Token, UnrecognizedTokenError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next;
        let slice = self.lexer.slice();
        for i in 0..LOOKAHEAD - 1 {
            self.next[i] = self.next[i + 1];
            self.slice[i] = self.slice[i + 1];
        }
        self.next[LOOKAHEAD - 1] = match self.lexer.next() {
            Some(Ok(next)) => Some(next),
            Some(Err(_)) => return Some(Err(UnrecognizedTokenError)),
            None => None,
        };
        self.slice[LOOKAHEAD - 1] = Some(slice);
        next[0].map(Ok)
    }
}
