use std::convert::TryInto;
use std::io::Cursor;

use crate::read_i16;

pub(crate) fn read_bytecode(parser: &mut Parser, table: &[usize]) {
    let mut elements = Vec::new();
    let mut entrypoint_marker = b'@';

    // Read bytecode
    while let Some(c) = parser.current() {
        if c == b'!' {
            entrypoint_marker = b'!';
        }

        // Read element
        let element = match c {
            0 | b',' => {
                parser.advance(1);
                Element::Comma
            }
            b'\n' => {
                parser.advance(1);
                let value = i16::from_le_bytes(parser.consume_n()) as usize;
                Element::Line(value)
            }
            b'@' | b'!' => {
                parser.advance(1);
                let value = i16::from_le_bytes(parser.consume_n());
                let val = table[value as usize];
                if val >= 1_000_000 {
                    Element::Entrypoint(val - 1_000_000)
                } else {
                    Element::Kidoku(val)
                }
            }
            b'$' => {
                let expr = parser.assign();
                Element::Expr(expr)
            }
            b'#' => {
                parser.advance(1);
                let element = read_function(&mut *parser, table);
                Element::Bytecode(Box::new(element))
            }
            _ => {
                let start = parser.pos;
                let mut quoted = false;
                while let Some(_) = parser.current() {
                    if quoted {
                        quoted = parser.current().unwrap() != b'"';
                        parser.consume_slice(b"\\\"");
                    } else {
                        parser.consume_exact(b',');
                        quoted = parser.current().unwrap() == b'"';
                        if matches!(parser.current().unwrap(), b'#' | b'$' | b'\n' | b'@') ||
                            parser.current().unwrap() == entrypoint_marker {
                            break;
                        }
                    }

                    if matches!(parser.current().unwrap(), 0x81..=0x9f | 0xe0..=0xef) {
                        parser.advance(2);
                    } else {
                        parser.advance(1);
                    }
                }
                dbg!(std::str::from_utf8(&parser.bc[start..parser.pos]));
                Element::Textout
            }
        };
        match &element {
            Element::Comma => println!("Comma"),
            Element::Entrypoint(ep) => println!("Entrypoint #{}", ep),
            Element::Kidoku(ep) => println!("Kidoku #{}", ep),
            Element::Line(ep) => println!("Line #{}", ep),
            Element::Expr(epr) => println!("{:?}", epr),
            Element::Bytecode(bc) => println!("Bytecode: {:?}", bc),
            Element::Textout => println!(">> {}", "TO"),
            Element::FunctionCall { .. } => todo!(),
            Element::Goto(_) => todo!(),
            Element::GotoIf(_, _) => todo!(),
        }
        elements.push(element);
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::{Parser, read_bytecode, read_function};

    #[test]
    fn assignment() {
        let mut p = Parser::new(&[
            0x24, 0x00, 0x5b, // 0x00 [
            0x24, 0xff, 0x01, 0x00, 0x00, 0x00, // const 1
            0x5d, // ]
            0x5c, 0x1e, // op=1e
            0x24, 0xff, 0x00, 0x00, 0x00, 0x00 // const 0
        ]);
        let expr = p.assign();
        dbg!(expr);
    }

    #[test]
    fn foo() {
        let mut p = Parser::new(&[
            0x23, 0x00, 0x01, 0x0c, 0x00, 0x02, 0x00, 0x01,
            0x28, // (
            0x24, 0xff, 0x75, 0x23, 0x00, 0x00,
            0x24, 0xff, 0x00, 0x00, 0x00, 0x00,
            0x29, // )
        ]);
        let e = read_bytecode(&mut p, &[]);
    }

    #[test]
    fn foo2() {
        let mut p = Parser::new(&[
            0x23, 0x01, 0x04, 0x72, 0x00, 0x00, 0x00, 0x01,
            0x24, 0x00, 0x5b,
            0x24, 0xff, 0x03, 0x00, 0x00, 0x00,
            0x5d,
            0x5c, 0x1e, 0x24, 0xc8
        ]);
        let e = read_function(&mut p, &[]);
    }
}

fn read_function(parser: &mut Parser, table: &[usize]) -> Element {
    // opcode: 0xttmmoooo (Type, Module, Opcode: e.g. 0x01030101 = 1:03:00257
    let opcode =
        ((parser.slice()[0] as u32) << 24) |
            ((parser.slice()[1] as u32) << 16) |
            ((parser.slice()[3] as u32) << 8) |
            (parser.slice()[2] as u32);

    match opcode {
        0x00010000 |
        0x00010005 |
        0x00050001 |
        0x00050005 |
        0x00060001 |
        0x00060005 => {
            let _chunk = parser.consume_n::<7>();
            let id = i32::from_le_bytes(parser.consume_n()) as usize;
            Element::Goto(id)
        }
        0x00010001 |
        0x00010002 |
        0x00010006 |
        0x00010007 |
        0x00050002 |
        0x00050006 |
        0x00050007 |
        0x00060000 |
        0x00060002 |
        0x00060006 |
        0x00060007 => {
            let _chunk = parser.consume_n::<7>();
            parser.expect(b'(');
            let p = parser.expr();
            parser.expect(b')');
            let id = i32::from_le_bytes(parser.consume_n()) as usize;
            Element::GotoIf(id, p)
        }
        0x00010003 |
        0x00010008 |
        0x00050003 |
        0x00050008 |
        0x00060003 |
        0x00060008 => panic!("gotoon"),
        0x00010004 |
        0x00010009 |
        0x00050004 |
        0x00050009 |
        0x00060004 |
        0x00060009 => panic!("gotocase"),
        0x00010010 |
        0x00060010 => panic!("gotosubwith"),
        0x00020000 |
        0x00020001 |
        0x00020002 |
        0x00020003 |
        0x00020010 => select(&mut *parser),
        oth => {
            let [modtype, module, opcode1, opcode2, argc1, argc2, overload] = parser.consume_n();
            let opcode = (opcode2 as u16) << 8 | (opcode1 as u16);
            let argc = (argc2 as u16) << 8 | (argc1 as u16);

            let mut params = Vec::new();
            if parser.consume_exact(b'(') {
                while parser.current() != Some(b')') {
                    params.push(parser.param());
                }
                parser.consume();
            }
            Element::FunctionCall { modtype, module, opcode, argc, params }
        }
    }
}

fn select(parser: &mut Parser) -> Element {
    let x = parser.consume_n::<7>();
    if parser.consume_exact(b'(') {
        let expr = parser.expr_term();
        dbg!(expr);
    }
    todo!()
}

pub(crate) struct Parser<'bc> {
    bc: &'bc [u8],
    pos: usize,
}

impl<'bc> Parser<'bc> {
    pub(crate) fn new(data: &[u8]) -> Parser {
        Parser {
            bc: data,
            pos: 0,
        }
    }

    fn current(&self) -> Option<u8> {
        self.bc.get(self.pos).copied()
    }

    #[track_caller]
    fn consume(&mut self) -> Option<u8> {
        let b = self.current()?;
        self.advance(1);
        Some(b)
    }

    #[track_caller]
    fn consume_if(&mut self, predicate: impl FnOnce(u8) -> bool) -> Option<u8> {
        let c = self.current()?;
        if predicate(c) {
            self.consume()
        } else {
            None
        }
    }

    #[track_caller]
    fn consume_exact(&mut self, b: u8) -> bool {
        self.consume_if(|v| v == b).is_some()
    }

    #[track_caller]
    fn consume_slice(&mut self, s: &[u8]) -> bool {
        if self.bc[self.pos..].starts_with(s) {
            self.advance(s.len());
            true
        } else {
            false
        }
    }

    #[track_caller]
    fn expect(&mut self, b: u8) {
        assert_eq!(self.consume(), Some(b));
    }

    fn assign(&mut self) -> Expr {
        let itok = self.expr_term();
        let _unknown = self.consume();
        let op = self.consume().unwrap();
        let etok = self.expr();
        if op >= 0x14 && op <= 0x24 {
            Expr::BinaryExpr(op, Box::new(itok), Box::new(etok))
        } else {
            panic!();
        }
    }

    fn expr_term(&mut self) -> Expr {
        if self.consume_slice(b"$\xff") {
            let val = self.consume_n();
            Expr::IntConst { value: i32::from_le_bytes(val) }
        } else if self.consume_slice(b"$\xc8") {
            Expr::StoreRegister
        } else if let [b'$', _, b'[', ..] = self.slice() {
            let [_, ty, _] = self.consume_n();
            let location = self.expr();
            self.expect(b']');
            Expr::MemRef(ty, Box::new(location))
        } else if self.consume_slice(b"\\\x00") {
            self.expr_term()
        } else if self.consume_slice(b"\\\x01") {
            let expr = self.expr_term();
            Expr::UnaryExpr(0x1, Box::new(expr))
        } else if self.consume_exact(b'(') {
            let expr = self.expr_bool();
            self.expect(b')');
            expr
        } else {
            self.dbg();
            panic!("unexpected {:?}", self.current());
        }
    }

    fn expr_bool(&mut self) -> Expr {
        let cond = self.expr_cond();
        let loop_and = self.expr_bool_loop_and(cond);
        self.expr_bool_loop_or(loop_and)
    }

    fn expr_bool_loop_or(&mut self, tok: Expr) -> Expr {
        if self.consume_slice(b"\\=") {
            let inner = self.expr_cond();
            let rhs = self.expr_bool_loop_and(inner);
            self.expr_bool_loop_or(Expr::BinaryExpr(0x3d, Box::new(tok), Box::new(rhs)))
        } else {
            tok
        }
    }

    fn expr_bool_loop_and(&mut self, tok: Expr) -> Expr {
        if self.consume_slice(b"\\<") {
            let rhs = self.expr_cond();
            self.expr_bool_loop_and(Expr::BinaryExpr(0x3c, Box::new(tok), Box::new(rhs)))
        } else {
            tok
        }
    }

    fn expr_cond(&mut self) -> Expr {
        let expr = self.expr_arithm();
        self.expr_cond_loop(expr)
    }

    fn slice(&self) -> &[u8] {
        &self.bc[self.pos..]
    }

    fn expr_cond_loop(&mut self, tok: Expr) -> Expr {
        if let [b'\\', op @ 0x28..=0x2d, ..] = self.slice() {
            let op = *op;
            self.advance(2);
            let rhs = self.expr_arithm();
            let new_piece = Expr::BinaryExpr(op, Box::new(tok), Box::new(rhs));
            self.expr_cond_loop(new_piece)
        } else {
            tok
        }
    }

    fn expr_arithm(&mut self) -> Expr {
        let expr = self.expr_term();
        let inner = self.expr_arithm_loop_hi_prec(expr);
        self.expr_arithm_loop(inner)
    }

    fn expr_arithm_loop(&mut self, tok: Expr) -> Expr {
        if let [b'\\', op @ (0x00 | 0x01)] = self.slice() {
            let op = *op;
            self.advance(2);
            let other = self.expr_term();
            let rhs = self.expr_arithm_loop_hi_prec(other);
            let new_piece = Expr::BinaryExpr(op, Box::new(tok), Box::new(rhs));
            self.expr_arithm_loop(new_piece)
        } else {
            tok
        }
    }

    fn expr_arithm_loop_hi_prec(&mut self, tok: Expr) -> Expr {
        if let [b'\\', op @ 0x02..=0x09] = self.slice() {
            let op = *op;
            self.advance(2);
            let new_piece = Expr::BinaryExpr(op, Box::new(tok), Box::new(self.expr_term()));
            self.expr_arithm_loop_hi_prec(new_piece)
        } else {
            tok
        }
    }

    fn expr(&mut self) -> Expr {
        self.expr_bool()
    }

    #[track_caller]
    fn consume_n<const N: usize>(&mut self) -> [u8; N] {
        let r = self.slice()[..N].try_into().unwrap();
        self.advance(N);
        r
    }

    #[track_caller]
    fn advance(&mut self, n: usize) {
        print!("{} {:04}: ", std::panic::Location::caller(), self.pos);
        for i in 0..n {
            print!("{:02x} ", self.slice().get(i).copied().unwrap_or(0))
        }
        println!();
        self.pos += n;
    }

    #[track_caller]
    fn dbg(&self) {
        println!("{} {:x} @ {}",
                 std::panic::Location::caller(),
                 self.current().unwrap_or(0), self.pos);
    }

    fn param(&mut self) -> Expr {
        if self.consume_exact(b',') {
            self.param()
        } else if self.consume_exact(b'\n') {
            self.advance(2);
            self.param()
        } else if is_string_char(self.current().unwrap()) || self.slice().starts_with(b"###PRINT(") {
            self.string()
        } else if let Some(c) = self.consume_if(|b| b == b'a' || b == b'(') {
            let mut exprs = Vec::new();

            if c == b'a' {
                let tag1 = self.consume();

                // Some special cases have multiple tags.
                let tag2 = if self.consume_exact(b'a') {
                    self.consume()
                } else {
                    None
                };
                let tag = (tag2.unwrap_or(0) as u16) << 8 |
                    (tag1.unwrap_or(0) as u16);

                if self.current() != Some(b')') {
                    exprs.push(self.param());
                    return Expr::SpecialExpr { tag, exprs };
                } else {
                    self.advance(1);
                }
            }

            while self.current() != Some(b')') {
                exprs.push(self.param());
            }

            Expr::ComplexExpr { exprs }
        } else {
            self.expr()
        }
    }

    fn string(&mut self) -> Expr {
        let start = self.pos;
        while let Some(c) = self.consume_if(|b| b.is_ascii_alphanumeric()) {
            // TODO: more characters + escaping
        }
        Expr::StringConst { value: std::str::from_utf8(&self.bc[start..self.pos]).unwrap().to_string() }
    }
}

fn is_string_char(b: u8) -> bool {
    matches!(b, 0x81..=0x9f | 0xe0..=0xef | b'A'..=b'Z' | b'0'..=b'9' | b' ' | b'?' | b'_' | b'"')
}

#[derive(Debug)]
enum Element {
    Comma,
    Entrypoint(usize),
    Kidoku(usize),
    Line(usize),
    Expr(Expr),
    Bytecode(Box<Element>),
    Textout,
    FunctionCall { modtype: u8, module: u8, opcode: u16, argc: u16, params: Vec<Expr> },
    Goto(usize),
    GotoIf(usize, Expr),
}

impl Element {
    fn len(&self) -> usize {
        match self {
            Element::Comma => 1,
            Element::Entrypoint(_) => 3,
            Element::Kidoku(_) => 3,
            Element::Line(_) => 3,
            Element::Expr(_) => todo!(),
            Element::Bytecode(_) => todo!(),
            Element::Textout => todo!(),
            Element::FunctionCall { .. } => todo!(),
            Element::Goto(_) => todo!(),
            Element::GotoIf(_, _) => todo!(),
        }
    }
}

enum Expr {
    StoreRegister,
    IntConst { value: i32 },
    StringConst { value: String },
    MemRef(u8, Box<Self>),
    SimpleMemRef,
    UnaryExpr(u8, Box<Self>),
    BinaryExpr(u8, Box<Self>, Box<Self>),
    SimpleAssignment,
    ComplexExpr { exprs: Vec<Expr> },
    SpecialExpr { tag: u16, exprs: Vec<Expr> },
    Command { params: Vec<Expr> },
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::StoreRegister => write!(f, "register")?,
            Expr::IntConst { value } => write!(f, "{}", value)?,
            Expr::StringConst { value } => write!(f, "{:?}", value)?,
            Expr::MemRef(place, index) => write!(f, "{}[{:?}]", match *place {
                int @ 0..=5 => (b'A' + int) as char,
                other => other as char,
            }, index)?,
            Expr::SimpleMemRef => todo!(),
            Expr::UnaryExpr(op, expr) => write!(f, "{}({:?})", op, expr)?,
            Expr::BinaryExpr(op, lhs, rhs) => write!(f, "{:?} {} {:?}", lhs, match *op {
                0 | 20 => "+",
                1 | 21 => "-",
                2 | 22 => "*",
                3 | 23 => "/",
                4 | 24 => "%",
                5 | 25 => "&",
                6 | 26 => "|",
                7 | 27 => "^",
                8 | 28 => "<<",
                9 | 29 => ">>",
                30 => "=",
                40 => "==",
                41 => "!=",
                42 => "<=",
                43 => "<",
                44 => ">=",
                45 => ">",
                60 => "&&",
                61 => "||",
                _ => unimplemented!(),
            }, rhs)?,
            Expr::SimpleAssignment => todo!(),
            Expr::ComplexExpr { .. } => todo!(),
            Expr::SpecialExpr { tag, exprs } => write!(f, "{}:{:?}", tag, exprs)?,
            Expr::Command { .. } => todo!(),
        }
        Ok(())
    }
}