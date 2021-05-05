use std::convert::TryInto;

const DEBUG: bool = cfg!(debug_assertions);

pub(crate) fn read_bytecode(parser: &mut Parser, table: &[usize]) -> Vec<(usize, Element)> {
    let mut elements = Vec::new();

    // Read bytecode
    while let Some(c) = parser.current() {
        if c == b'!' {
            parser.entrypoint_marker = b'!';
        }

        let start = parser.pos;

        // Read element
        let element = match c {
            0 | b',' => {
                parser.advance(1);
                Element::Halt
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
                read_function(parser)
            }
            _ => {
                let mut s = Vec::new();
                let mut is_quoted = false;
                while let Some(c) = parser.current() {
                    match c {
                        b'"' => {
                            parser.advance(1);
                            s.push(c);
                            if is_quoted {
                                break;
                            }
                            is_quoted = !is_quoted;
                            continue;
                        }
                        b'\\' if is_quoted => {
                            parser.advance(1);
                            s.push(parser.consume().unwrap());
                        }
                        b'#' | b'$' | b'\n' | b'@' | 0 if !is_quoted => break,
                        c if c == parser.entrypoint_marker && !is_quoted => break,
                        _ => {
                            // CP932 encoding bytes
                            if let 0x81..=0x9f | 0xe0..=0xef = c {
                                parser.advance(1);
                                s.push(c);
                            }
                            s.push(parser.consume().unwrap());
                        }
                    }
                }
                assert!(!s.is_empty());
                Element::Textout(encoding_rs::SHIFT_JIS.decode(&s).0.into_owned().into_boxed_str())
            }
        };
        if DEBUG {
            match &element {
                Element::Halt => println!("Halt"),
                Element::Entrypoint(ep) => println!("Entrypoint #{}", ep),
                Element::Kidoku(ep) => println!("Kidoku #{}", ep),
                Element::Line(ep) => println!("Line #{}", ep),
                Element::Expr(epr) => println!("{:?}", epr),
                Element::Textout(s) => println!(">> {}", s),
                Element::FunctionCall { params: _, meta: _ } => println!("{:?}", element),
                Element::GoSubWith { target: _, meta: _, params: _ } => println!("{:?}", element),
                Element::Goto { target: _ } => println!("{:?}", element),
                Element::GotoIf { target: _, cond: _ } => println!("{:?}", element),
                Element::Select { cond: _, params, first_line: _ } => println!("select: {:?}", params),
            }
        }
        elements.push((start, element));
    }
    elements
}

#[cfg(test)]
mod tests {
    use crate::parse::{Element, Parser, read_bytecode};

    fn repr(x: &impl std::fmt::Debug) -> String {
        use std::fmt::Write;
        let mut s = String::new();
        write!(&mut s, "{:?}", x).unwrap();
        s
    }

    fn parse_element(s: &[u8]) -> Element {
        let mut p = Parser::new(s);
        let result = read_bytecode(&mut p, &[]);
        assert_eq!(result.len(), 1);
        result
            .into_iter()
            .next()
            .unwrap()
            .1
    }

    #[test]
    fn assignment() {
        let e = parse_element(&[
            0x24, 0x00, 0x5b, // 0x00 [
            0x24, 0xff, 0x01, 0x00, 0x00, 0x00, // const 1
            0x5d, // ]
            0x5c, 0x1e, // op=1e
            0x24, 0xff, 0x00, 0x00, 0x00, 0x00 // const 0
        ]);
        assert_eq!(repr(&e), "Expr(A[1] = 0)");
    }

    #[test]
    fn call_with_args() {
        let e = parse_element(&[
            0x23, 0x00, 0x01, 0x0c, 0x00, 0x02, 0x00, 0x01,
            0x28, // (
            0x24, 0xff, 0x75, 0x23, 0x00, 0x00,
            0x24, 0xff, 0x00, 0x00, 0x00, 0x00,
            0x29, // )
        ]);
        assert_eq!(repr(&e), "FunctionCall { meta: CallMeta { type: 0, module: 1, opcode: 12, argc: 2, overload: 1 }, params: [9077, 0] }");
    }

    #[test]
    fn call_without_args() {
        let e = parse_element(&[
            0x23, 0x01, 0x04, 0x72, 0x00, 0x00, 0x00, 0x01
        ]);
        assert_eq!(repr(&e), "FunctionCall { meta: CallMeta { type: 1, module: 4, opcode: 114, argc: 0, overload: 1 }, params: [] }");
    }

    #[test]
    fn assign_from_register() {
        let e = parse_element(&[
            0x24, 0x00, 0x5b,
            0x24, 0xff, 0x03, 0x00, 0x00, 0x00,
            0x5d,
            0x5c, 0x1e, 0x24, 0xc8
        ]);
        assert_eq!(repr(&e), "Expr(A[3] = register)");
    }

    #[test]
    fn call_with_string_arg() {
        let e = parse_element(&[
            0x23, 0x01, 0x0A, 0x00, 0x00, 0x02, 0x00, 0x00,
            0x28, // (
            0x24, 0x12, 0x5B, 0x24, 0xFF, 0xE9, 0x03, 0x00, 0x00, 0x5D,
            0x2C, // ,
            0x4E, 0x4F, 0x4E, 0x45, // NONE
            0x29, // )
        ]);
        assert_eq!(repr(&e), "FunctionCall { meta: CallMeta { type: 1, module: 10, opcode: 0, argc: 2, overload: 0 }, params: [strS[1001], \"NONE\"] }");
    }

    #[test]
    fn select() {
        let e = parse_element(&[
            0x23, 0x00, 0x02, 0x01, 0x00, 0x02, 0x00, 0x00,
            0x7B, 0x0A, 0xBA, 0x04, 0x22, 0x53, 0x6B, 0x69,
            0x70, 0x20, 0x63, 0x6C, 0x61, 0x73, 0x73, 0x22,
            0x0A, 0xBB, 0x04, 0x22, 0x53, 0x74, 0x61, 0x79,
            0x20, 0x61, 0x72, 0x6F, 0x75, 0x6E, 0x64, 0x22,
            0x0A, 0xBC, 0x04, 0x7D,
        ]);
        assert_eq!(repr(&e), r#"Select { cond: None, params: [SelectOption { line: 1211, text: "\"Skip class\"" }, SelectOption { line: 1212, text: "\"Stay around\"" }], first_line: 1210 }"#);
    }

    #[test]
    fn textout() {
        let e = parse_element(&[
            0x22, 0x54, 0x68, 0x65, 0x20, 0x67, 0x72,
            0x6F, 0x75, 0x6E, 0x64, 0x20, 0x62, 0x65, 0x67,
            0x69, 0x6E, 0x73, 0x20, 0x74, 0x6F, 0x20, 0x72,
            0x75, 0x6D, 0x62, 0x6C, 0x65, 0x20, 0x61, 0x67,
            0x61, 0x69, 0x6E, 0x00, 0x22,
        ]);
        assert_eq!(repr(&e), r#"Textout("\"The ground begins to rumble again\u{0}\"")"#);
    }

    #[test]
    fn complex_select() {
        let e = parse_element(&[
            0x23, 0x00, 0x02, 0x03, 0x00, 0x02, 0x00, 0x00,
            0x7B,
            0x0A, 0x13, 0x00,
            0x23, 0x23, 0x23, 0x50, 0x52, 0x49, 0x4E, 0x54,
            0x28, 0x24, 0x12, 0x5B, 0x24, 0xFF, 0xF3, 0x03, 0x00, 0x00, 0x5D, 0x29,
            0x0A, 0x14, 0x00,
            0x23, 0x23, 0x23, 0x50, 0x52, 0x49, 0x4E, 0x54,
            0x28, 0x24, 0x12, 0x5B, 0x24, 0xFF, 0xF4, 0x03, 0x00, 0x00, 0x5D, 0x29,
            0x0A, 0x15, 0x00,
            0x7D
        ]);
        assert_eq!(repr(&e), r#"Select { cond: None, params: [SelectOption { line: 20, text: strS[1011] }, SelectOption { line: 21, text: strS[1012] }], first_line: 19 }"#);
    }
}

fn read_function(parser: &mut Parser) -> Element {
    // opcode: 0xttmmoooo (Type, Module, Opcode: e.g. 0x01030101 = 1:03:00257
    let opcode =
        ((parser.slice()[0] as u32) << 24) |
            ((parser.slice()[1] as u32) << 16) |
            ((parser.slice()[3] as u32) << 8) |
            ((parser.slice()[2] as u32) << 0);

    let meta = read_call_meta(parser);
    match opcode {
        0x00010000 |
        0x00010005 |
        0x00050001 |
        0x00050005 |
        0x00060001 |
        0x00060005 => {
            let target = i32::from_le_bytes(parser.consume_n()) as usize;
            Element::Goto { target }
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
            parser.expect(b'(');
            let cond = parser.expr();
            parser.expect(b')');
            let target = i32::from_le_bytes(parser.consume_n()) as usize;
            Element::GotoIf { target, cond }
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
        0x00060010 => {
            let mut params = Vec::new();
            if parser.consume_exact(b'(') {
                while parser.current() != Some(b')') {
                    params.push(parser.param());
                }
                parser.consume();
            }
            let target = i32::from_le_bytes(parser.consume_n()) as usize;
            Element::GoSubWith { target, meta, params: params.into_boxed_slice() }
        }
        0x00020000 |
        0x00020001 |
        0x00020002 |
        0x00020003 |
        0x00020010 => select(parser, meta),
        _ => {
            // Other opcodes
            let mut params = Vec::new();
            if parser.consume_exact(b'(') {
                while parser.current() != Some(b')') {
                    params.push(parser.param());
                }
                parser.consume();
            }
            Element::FunctionCall { meta, params: params.into_boxed_slice() }
        }
    }
}

#[derive(Debug)]
pub struct CallMeta {
    r#type: u8,
    module: u8,
    pub opcode: u16,
    argc: u16,
    overload: u8,
}

fn read_call_meta(parser: &mut Parser) -> CallMeta {
    let [r#type, module, opcode1, opcode2, argc1, argc2, overload] = parser.consume_n();
    let opcode = (opcode2 as u16) << 8 | (opcode1 as u16);
    let argc = (argc2 as u16) << 8 | (argc1 as u16);
    CallMeta {
        r#type,
        module,
        opcode,
        argc,
        overload,
    }
}

fn select(parser: &mut Parser, meta: CallMeta) -> Element {
    let cond = if parser.consume_exact(b'(') {
        Some(parser.expr_term())
    } else {
        None
    };
    parser.expect(b'{');
    let first_line = if parser.consume_exact(b'\n') {
        i16::from_le_bytes(parser.consume_n()) as u16
    } else {
        0
    };

    let mut params = Vec::new();
    for _ in 0..meta.argc {
        // Skip preliminary metadata.
        while parser.consume_exact(b',') {}

        let mut conds = Vec::new();
        // Read condition, if present.
        if parser.consume_exact(b'(') {
            while parser.current() != Some(b')') {
                let cond = parser.expr();
                conds.push(cond);
            }
            parser.expect(b')');
        }

        let text = if parser.consume_slice(b"###PRINT(") {
            let expr = parser.expr();
            parser.expect(b')');
            expr
        } else {
            // Read text
            let text = parser.string();
            Expr::StringConst { value: text.into_boxed_str() }
        };

        parser.expect(b'\n');
        let line_number = i16::from_le_bytes(parser.consume_n());
        params.push(SelectOption {
            line: line_number as u16,
            text,
        });
    }

    while parser.consume_exact(b'\n') {
        // The only thing allowed other than a 16 bit integer.
        parser.advance(2);
    }
    parser.expect(b'}');
    Element::Select { cond: cond.map(Box::new), params: params.into_boxed_slice(), first_line }
}

pub(crate) struct Parser<'bc> {
    data: &'bc [u8],
    pos: usize,
    entrypoint_marker: u8,
}

impl<'bc> Parser<'bc> {
    pub(crate) fn new_at(data: &'bc [u8], pos: usize) -> Parser<'bc> {
        Self { data, pos, entrypoint_marker: b'@' }
    }

    #[allow(unused)]
    pub(crate) fn new(data: &'bc [u8]) -> Parser<'bc> {
        Self::new_at(data, 0)
    }

    #[inline]
    fn current(&self) -> Option<u8> {
        self.data.get(self.pos).copied()
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
        if self.data[self.pos..].starts_with(s) {
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
        let lhs = self.expr_term();
        self.expect(0x5c);
        let op = self.consume().unwrap();
        let rhs = self.expr();
        if op >= 0x14 && op <= 0x24 {
            let op = match op {
                0x14 => Operator::Plus,
                0x15 => Operator::Minus,
                0x16 => Operator::Asterisk,
                0x17 => Operator::Slash,
                0x18 => Operator::Percent,
                0x19 => Operator::BitAnd,
                0x1A => Operator::BitOr,
                0x1B => Operator::BitXor,
                0x1C => Operator::ShiftLeft,
                0x1D => Operator::ShiftRight,
                0x1E => Operator::Assign,
                _ => unimplemented!("unsupported operator: {}", op),
            };

            Expr::Binary { op, lhs: Box::new(lhs), rhs: Box::new(rhs) }
        } else {
            panic!();
        }
    }

    fn expr_term(&mut self) -> Expr {
        if self.consume_exact(b'$') {
            if self.consume_exact(0xff) {
                Expr::IntConst { value: i32::from_le_bytes(self.consume_n()) }
            } else if self.consume_exact(0xc8) {
                Expr::StoreRegister
            } else {
                let bank = self.consume().unwrap();
                self.expect(b'[');
                let location = self.expr();
                self.expect(b']');
                Expr::MemRef { bank, location: Box::new(location) }
            }
        } else if self.consume_exact(b'\\') {
            let op = self.consume().unwrap();
            let expr = self.expr_term();
            let op = match op {
                0x00 => Operator::Plus,
                0x01 => Operator::Minus,
                _ => unimplemented!(),
            };
            Expr::Unary { op, expr: Box::new(expr) }
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
            self.expr_bool_loop_or(Expr::Binary { op: Operator::Or, lhs: Box::new(tok), rhs: Box::new(rhs) })
        } else {
            tok
        }
    }

    fn expr_bool_loop_and(&mut self, tok: Expr) -> Expr {
        if self.consume_slice(b"\\<") {
            let rhs = self.expr_cond();
            self.expr_bool_loop_and(Expr::Binary { op: Operator::And, lhs: Box::new(tok), rhs: Box::new(rhs) })
        } else {
            tok
        }
    }

    fn expr_cond(&mut self) -> Expr {
        let expr = self.expr_arithm();
        self.expr_cond_loop(expr)
    }

    fn slice(&self) -> &[u8] {
        &self.data[self.pos..]
    }

    fn expr_cond_loop(&mut self, tok: Expr) -> Expr {
        if let &[b'\\', op @ 0x28..=0x2d, ..] = self.slice() {
            self.advance(2);
            let rhs = self.expr_arithm();
            let op = match op {
                0x28 => Operator::Equal,
                0x29 => Operator::NotEqual,
                0x2A => Operator::LessEqual,
                0x2B => Operator::Less,
                0x2C => Operator::GreaterEqual,
                0x2D => Operator::Greater,
                _ => unreachable!(),
            };
            let new_piece = Expr::Binary { op, lhs: Box::new(tok), rhs: Box::new(rhs) };
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
        if let &[b'\\', op @ (0x00 | 0x01), ..] = self.slice() {
            self.advance(2);
            let other = self.expr_term();
            let rhs = self.expr_arithm_loop_hi_prec(other);
            let op = match op {
                0x00 => Operator::Plus,
                0x01 => Operator::Minus,
                _ => unreachable!(),
            };
            let new_piece = Expr::Binary { op, lhs: Box::new(tok), rhs: Box::new(rhs) };
            self.expr_arithm_loop(new_piece)
        } else {
            tok
        }
    }

    fn expr_arithm_loop_hi_prec(&mut self, tok: Expr) -> Expr {
        if let &[b'\\', op @ 0x02..=0x09, ..] = self.slice() {
            self.advance(2);
            let op = match op {
                0x02 => Operator::Asterisk,
                0x03 => Operator::Slash,
                0x04 => Operator::Percent,
                0x05 => Operator::BitAnd,
                0x06 => Operator::BitOr,
                0x07 => Operator::BitXor,
                0x08 => Operator::ShiftLeft,
                0x09 => Operator::ShiftRight,
                _ => unreachable!(),
            };
            let new_piece = Expr::Binary { op, lhs: Box::new(tok), rhs: Box::new(self.expr_term()) };
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
        if DEBUG {
            print!("{} {:04}: ", std::panic::Location::caller(), self.pos);
            for i in 0..n {
                print!("{:02x} ", self.slice().get(i).copied().unwrap_or(0))
            }

            print!(" | ");
            for c in self.slice()[..n].iter().map(|c| *c as char) {
                if c.is_ascii_control() {
                    print!("  ");
                } else {
                    print!("'{}' ", c);
                }
            }

            println!();
        }

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
        } else if self.consume_slice(b"###PRINT(") {
            let expr = self.expr();
            self.expect(b')');
            expr
        } else if is_string_char(self.current().unwrap()) {
            // Read text
            let text = self.string();
            Expr::StringConst { value: text.into_boxed_str() }
        } else if self.consume_exact(b'a') {
            let mut exprs = Vec::new();

            let mut tag = self.consume().unwrap() as u32;
            if self.consume_exact(b'a') {
                // Some special cases have multiple tags.
                let tag2 = self.consume().unwrap();
                tag = (tag2 as u32) << 16 | (tag as u32);
            }

            if self.consume_exact(b'(') {
                while self.current() != Some(b')') {
                    exprs.push(self.param());
                }
                self.expect(b')');
            } else {
                exprs.push(self.param());
            }

            Expr::Special { tag, exprs: exprs.into_boxed_slice() }
        } else {
            self.expr()
        }
    }

    fn string(&mut self) -> String {
        let mut buffer = Vec::new();
        let mut is_quoted = false;
        while let Some(c) = self.current() {
            match c {
                b'"' => {
                    self.advance(1);
                    buffer.push(c);
                    if is_quoted {
                        break;
                    } else {
                        is_quoted = !is_quoted;
                        continue;
                    }
                }
                b'\\' if is_quoted => {
                    self.advance(1);
                    buffer.push(self.consume().unwrap());
                }
                0x81..=0x9f | 0xe0..=0xef => {
                    self.advance(1);
                    // CP936 crap
                    buffer.push(c);
                    buffer.push(self.consume().unwrap());
                }
                _ if is_quoted => {
                    self.advance(1);
                    buffer.push(c);
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b' ' | b'?' | b'_' => {
                    self.advance(1);
                    buffer.push(c);
                }
                _ => break,
            }
        }
        assert!(!buffer.is_empty());

        encoding_rs::SHIFT_JIS.decode(&buffer).0.into_owned()
    }
}

fn is_string_char(b: u8) -> bool {
    matches!(b, 0x81..=0x9f | 0xe0..=0xef | b'A'..=b'Z' | b'0'..=b'9' | b' ' | b'?' | b'_' | b'"')
}

#[derive(Debug)]
pub enum Element {
    Halt,
    Entrypoint(usize),
    Kidoku(usize),
    Line(usize),
    Expr(Expr),
    Textout(Box<str>),
    FunctionCall { meta: CallMeta, params: Box<[Expr]> },
    GoSubWith { target: usize, meta: CallMeta, params: Box<[Expr]> },
    Goto { target: usize },
    GotoIf { cond: Expr, target: usize },
    Select { cond: Option<Box<Expr>>, params: Box<[SelectOption]>, first_line: u16 },
}

#[derive(Debug)]
pub struct SelectOption {
    line: u16,
    pub(crate) text: Expr,
}

pub enum Expr {
    StoreRegister,
    IntConst { value: i32 },
    StringConst { value: Box<str> },
    MemRef { bank: u8, location: Box<Self> },
    Unary { op: Operator, expr: Box<Self> },
    Binary { op: Operator, lhs: Box<Self>, rhs: Box<Self> },
    Special { tag: u32, exprs: Box<[Self]> },
}

#[derive(Debug)]
#[repr(u8)]
pub enum Operator {
    Equal,
    NotEqual,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,
    And,
    Or,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    Assign,
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::StoreRegister => write!(f, "register")?,
            Expr::IntConst { value } => write!(f, "{}", value)?,
            Expr::StringConst { value } => write!(f, "{:?}", value)?,
            Expr::MemRef { bank, location } => write!(f, "{}[{:?}]", match bank {
                int @ 0..=6 => ((b'A' + int) as char).to_string(),
                intzl @ 7..=8 => ((b'A' + intzl) as char).to_string(),
                0x0a => "strK".to_string(),
                0x0C => "strM".to_string(),
                0x12 => "strS".to_string(),
                25 => "intZ".to_string(),
                11 => "intL".to_string(),
                other => todo!("{}", other),
            }, location)?,
            Expr::Unary { op, expr } => write!(f, "{}{:?}", match op {
                Operator::Plus => "+",
                Operator::Minus => "-",
                _ => unimplemented!()
            }, expr)?,
            Expr::Binary { op, lhs, rhs } => write!(f, "{:?} {} {:?}", lhs, match *op {
                Operator::Equal => "==",
                Operator::NotEqual => "!=",
                Operator::LessEqual => "<=",
                Operator::Less => "<",
                Operator::GreaterEqual => ">=",
                Operator::Greater => ">",
                Operator::And => "&&",
                Operator::Or => "||",
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Asterisk => "*",
                Operator::Slash => "/",
                Operator::Percent => "%",
                Operator::BitAnd => "&",
                Operator::BitOr => "|",
                Operator::BitXor => "^",
                Operator::ShiftLeft => "<<",
                Operator::ShiftRight => ">>",
                Operator::Assign => "=",
            }, rhs)?,
            Expr::Special { tag, exprs } => {
                write!(f, "{}:{{", tag)?;
                let mut first = true;
                for e in exprs.iter() {
                    if !first {
                        write!(f, ",")?;
                    } else {
                        first = false;
                    }
                    write!(f, "{:?}", e)?;
                }
                write!(f, "}}")?;
            }
        }
        Ok(())
    }
}