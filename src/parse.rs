use crate::read_i16;
use std::convert::TryInto;

pub(crate) fn read_bytecode(bc: &[u8], table: &[usize]) {
    let mut elements = Vec::new();
    let mut entrypoint_marker = b'@';

    // Read bytecode
    let mut pos = 0;
    while pos < bc.len() {
        // Read element
        if bc[pos] == b'!' {
            entrypoint_marker = b'!';
        }

        let element = match dbg!(bc[pos]) {
            0 | b',' => {
                Element::Comma
            }
            b'\n' => {
                let value = read_i16(&bc[pos + 1..]) as usize;
                Element::Line(value)
            }
            b'@' | b'!' => {
                let value = read_i16(&bc[pos + 1..]);
                let val = table[value as usize];
                if val >= 1_000_000 {
                    Element::Entrypoint(val - 1_000_000)
                } else {
                    Element::Kidoku(val)
                }
            }
            b'$' => {
                let mut x = Parser::new(&bc[pos..]);
                dbg!(x.assign());

                Element::Expr
            }
            b'#' => {
                read_function(&bc[pos..]);
                Element::Bytecode
            }
            _ => {
                let mut end = pos;
                let mut quoted = false;
                while end < bc.len() {
                    if quoted {
                        quoted = bc[end] != b'"';
                        if bc[end] == b'\\' && bc[end + 1] == b'"' {
                            end += 1;
                        }
                    } else {
                        if bc[end] == b',' {
                            end += 1;
                        }

                        quoted = bc[end] == b'"';
                        if matches!(bc[end], b'#' | b'$' | b'\n' | b'@') || bc[end] == entrypoint_marker {
                            break;
                        }
                    }

                    if matches!(bc[end], 0x81..=0x9f | 0xe9..=0xef) {
                        end += 2;
                    } else {
                        end += 1;
                    }
                }

                dbg!(std::str::from_utf8(&bc[pos..end]));

                panic!();
                Element::Textout
            }
        };
        pos += element.len();
        elements.push(dbg!(element));
    }

    dbg!(elements);
}

fn read_function(code: &[u8]) {
    // opcode: 0xttmmoooo (Type, Module, Opcode: e.g. 0x01030101 = 1:03:00257
    let opcode =
        ((code[1] as u32) << 24) |
            ((code[2] as u32) << 16) |
            ((code[4] as u32) << 8) |
            (code[3] as u32);
    match opcode {
        0x00010000 |
        0x00010005 |
        0x00050001 |
        0x00050005 |
        0x00060001 |
        0x00060005 => panic!("goto"),
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
        0x00060007 => panic!("gotoif"),
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
        0x00020010 => panic!("select"),
        _ => {
            if code[8] == b'(' {

            }

            panic!("{:?}", std::str::from_utf8(&code[8..10]))
        },
    }
}

struct Parser<'bc> {
    bc: &'bc [u8],
    pos: usize,
}

impl<'bc> Parser<'bc> {
    fn new(data: &[u8]) -> Parser {
        Parser {
            bc: data,
            pos: 0,
        }
    }

    fn current(&self) -> Option<u8> {
        self.bc.get(self.pos).copied()
    }

    fn consume(&mut self) -> Option<u8> {
        let b = self.current()?;
        self.pos += 1;
        Some(b)
    }

    fn consume_if(&mut self, predicate: impl FnOnce(u8) -> bool) -> Option<u8> {
        let c = self.current()?;
        if predicate(c) {
            self.consume()
        } else {
            None
        }
    }

    fn consume_exact(&mut self, b: u8) -> Option<u8> {
        self.consume_if(|v| v == b)
    }

    fn consume_slice(&mut self, s: &[u8]) -> bool {
        if &self.bc[self.pos..] == s {
            self.pos += s.len();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, b: u8) {
        assert_eq!(self.current(), Some(b))
    }

    fn assign(&mut self) -> Expr {
        let itok = self.expr_term();
        let op = self.slice()[1];
        self.pos += 2;
        let etok = self.expr();
        if op >= 0x14 && op <= 0x24 {
            Expr::BinaryExpr(op, Box::new(itok), Box::new(etok))
        } else {
            panic!();
        }
    }

    fn expr_term(&mut self) -> Expr {
        if self.consume_exact(b'$').is_some() {
            self.expr_token()
        } else if self.consume_slice(b"\n\x00") {
            self.expr_term()
        } else if self.consume_slice(b"\n\x01") {
            let expr = self.expr_term();
            Expr::UnaryExpr(0x1, Box::new(expr))
        } else if self.consume_exact(b'(').is_some() {
            let expr = self.expr_bool();
            self.expect(b')');
            expr
        } else {
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
        if let [b'\\', op @ 0x28..=0x2d] = self.slice() {
            let op = *op;
            self.pos += 2;
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
            self.pos += 2;
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
            self.pos += 2;
            let new_piece = Expr::BinaryExpr(op, Box::new(tok), Box::new(self.expr_term()));
            self.expr_arithm_loop_hi_prec(new_piece)
        } else {
            tok
        }
    }

    fn expr(&mut self) -> Expr {
        self.expr_bool()
    }

    fn consume_n<const N: usize>(&mut self) -> [u8; N] {
        self.bc[self.pos..][..N].try_into().unwrap()
    }

    fn expr_token(&mut self) -> Expr {
        if self.consume_exact(0xff).is_some() {
            Expr::IntConst { value: i32::from_le_bytes(self.consume_n()) }
        } else if self.consume_exact(0xc8).is_some() {
            Expr::StoreRegister
        } else if let [ty, b']'] = self.consume_n() {
            let location = self.expr();
            if self.current() != Some(b']') {
                panic!("unexpected");
            }
            Expr::MemRef(ty, Box::new(location))
        } else {
            panic!("unexpected {:?}", self.current());
        }
    }
}


// fn assignment(s: &[u8]) -> (Expr, &[u8]) {
//     let (itok, s) = expr_term(s);
//     let op = s[1];
//     let s = &s[2..];
//     let (etok, s) = expr(s);
//     if op >= 0x14 && op <= 0x24 {
//         (Expr::BinaryExpr(op, Box::new(itok), Box::new(etok)), s)
//     } else {
//         panic!("undefined assignment")
//     }
// }
//
// fn expr(s: &[u8]) -> (Expr, &[u8]) {
//     expr_bool(s)
// }
//
// fn expr_bool(s: &[u8]) -> (Expr, &[u8]) {
//     expr_bool_loop_or(src)
// }
//
// fn expr_bool_loop_or(s: &[u8]) -> (Expr, &[u8]) {
//     match s {
//         [b'\\', b'=', s @ ..] => {}
//     }
// }

#[derive(Debug)]
enum Element {
    Comma,
    Entrypoint(usize),
    Kidoku(usize),
    Line(usize),
    Expr,
    Bytecode,
    Textout,
}

impl Element {
    fn len(&self) -> usize {
        match self {
            Element::Comma => 1,
            Element::Entrypoint(_) => 3,
            Element::Kidoku(_) => 3,
            Element::Line(_) => 3,
            Element::Expr => todo!(),
            Element::Bytecode => todo!(),
            Element::Textout => todo!(),
        }
    }
}


//
// fn expr_token() -> (Expr, &[u8]) {
//     todo!()
// }


//
// fn next_token(c: &mut Cursor) {
//
// }
//
//
#[derive(Debug)]
enum Expr {
    StoreRegister,
    IntConst { value: i32 },
    StringConst { value: String },
    MemRef(u8, Box<Self>),
    SimpleMemRef,
    UnaryExpr(u8, Box<Self>),
    BinaryExpr(u8, Box<Self>, Box<Self>),
    SimpleAssignment,
    ComplexExpr,
    SpecialExpr,
}