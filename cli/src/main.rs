#![feature(array_map)]

use std::cmp::Reverse;

use reallive::{Element, Expr, read_archive, CallMeta, Operator};
use std::collections::HashMap;
use std::borrow::Cow;
use std::fmt::Formatter;

type MyError = Box<dyn std::error::Error>;

struct Memory {
    int_banks: [Vec<i32>; 6],
    str_banks: [Vec<String>; 2],
    register: i32,
}

impl Memory {
    fn new() -> Self {
        Self {
            int_banks: [(); 6].map(|_| vec![0; 10000]),
            str_banks: [(); 2].map(|_| vec![String::new(); 10000]),
            register: 0,
        }
    }

    fn get(&self, bank: u8, location: usize) -> Value<'static> {
        match bank {
            0..=6 => Value::Int(self.int_banks[bank as usize][location]),
            18 => Value::Str(self.str_banks[0][location].clone().into()),
            _ => todo!("getting {}[{}]", bank, location),
        }
    }

    fn set(&mut self, bank: u8, location: usize, value: Value) {
        match (bank, value) {
            (0..=6, Value::Int(x)) => {
                self.int_banks[bank as usize][location] = x;
            }
            (bank, value) => todo!("setting {}[{}] to {:?}", bank, location, value),
        }
    }
}

struct Machine {
    pointer: usize,
    scenario: u32,
    memory: Memory,
}

impl Machine {
    fn new(scenario: u32) -> Self {
        Self {
            pointer: 0,
            scenario,
            memory: Memory::new(),
        }
    }
}

#[derive(Clone)]
enum Value<'s> {
    Str(Cow<'s, str>),
    Int(i32),
    Bool(bool),
}

impl<'s> std::fmt::Debug for Value<'s> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Int(s) => write!(f, "{}", s),
            Value::Bool(s) => write!(f, "{:?}", s),
        }
    }
}

impl<'s> Value<'s> {
    fn as_integer(&self) -> Option<i32> {
        match self {
            Value::Str(_) | Value::Bool(_) => None,
            Value::Int(s) => Some(*s),
        }
    }

    fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Str(_) | Value::Int(_) => None,
            Value::Bool(x) => Some(*x),
        }
    }
}

#[derive(Debug)]
enum StepResult<'bc> {
    Continue,
    Halt,
    Text(Cow<'bc, str>),
    Call(&'bc CallMeta, Vec<Value<'bc>>),
    Exit,
}

fn evaluate_expr<'s>(expr: &'s Expr, machine: &mut Machine) -> Value<'s> {
    match expr {
        Expr::StoreRegister => Value::Int(machine.memory.register),
        Expr::StringConst { value } => Value::Str(Cow::Borrowed(value)),
        Expr::IntConst { value } => Value::Int(*value),
        Expr::MemRef { bank, location } => {
            let location = evaluate_expr(location, machine).as_integer().unwrap() as usize;
            machine.memory.get(*bank, location)
        }
        Expr::Binary { op, lhs, rhs } => {
            if let Operator::Assign = op {
                match lhs.as_ref() {
                    Expr::MemRef { bank, location } => {
                        let location = evaluate_expr(location, machine).as_integer().unwrap() as usize;
                        let value = evaluate_expr(rhs, machine);
                        machine.memory.set(*bank, location, value.clone());
                        return value;
                    }
                    _ => todo!(),
                }
            } else {
                match (evaluate_expr(lhs, machine), evaluate_expr(rhs, machine)) {
                    (Value::Int(lhs), Value::Int(rhs)) => {
                        match op {
                            Operator::Asterisk => Value::Int(lhs * rhs),
                            Operator::Plus => Value::Int(lhs + rhs),
                            Operator::Minus => Value::Int(lhs - rhs),
                            Operator::Slash => Value::Int(lhs / rhs),
                            Operator::Greater => Value::Bool(lhs > rhs),
                            Operator::GreaterEqual => Value::Bool(lhs >= rhs),
                            Operator::LessEqual => Value::Bool(lhs <= rhs),
                            Operator::Equal => Value::Bool(lhs == rhs),
                            _ => todo!("{:?}", op),
                        }
                    }
                    (Value::Bool(lhs), Value::Bool(rhs)) => {
                        match op {
                            Operator::And => Value::Bool(lhs && rhs),
                            Operator::Or => Value::Bool(lhs || rhs),
                            _ => todo!("{:?} on bool", op),
                        }
                    }
                    _ => todo!(),
                }
            }
        }
        Expr::Unary { op, expr } => {
            let value = evaluate_expr(expr, machine).as_integer().unwrap();
            match op {
                Operator::Minus => Value::Int(-value),
                _ => todo!("{:?}", op),
            }
        }
        Expr::Special { .. } => Value::Str("".into()),
    }
}

fn step<'s>(machine: &mut Machine, scenarios: &'s [Scenario]) -> StepResult<'s> {
    let idx = scenarios.binary_search_by_key(&machine.scenario, |x| x.id).unwrap();
    let scenario = &scenarios[idx];
    let (pos, inst) = match scenario.elements.get(machine.pointer) {
        Some(x) => x,
        None => return StepResult::Exit,
    };

    match inst {
        Element::Halt => {
            machine.pointer += 1;
            StepResult::Halt
        }
        Element::Entrypoint(_) => {
            machine.pointer += 1;
            StepResult::Continue
        }
        Element::Kidoku(_) => {
            machine.pointer += 1;
            StepResult::Continue
        }
        Element::Line(_) => {
            machine.pointer += 1;
            StepResult::Continue
        }
        Element::Expr(e) => {
            evaluate_expr(e, machine);
            machine.pointer += 1;
            StepResult::Continue
        }
        Element::Textout(text) => {
            machine.pointer += 1;
            StepResult::Text(Cow::Borrowed(text))
        }
        Element::FunctionCall { meta, params } => {
            let ret = StepResult::Call(meta, params.iter().map(|p| evaluate_expr(p, machine)).collect());
            machine.pointer += 1;
            ret
        }
        Element::GoSubWith { target, .. } => todo!(),
        Element::Goto { target } => {
            machine.pointer = scenario.elements.partition_point(|p| p.0 < *target);
            StepResult::Continue
        }
        Element::GotoIf { target, cond } => {
            let cond = evaluate_expr(cond, machine).as_bool().unwrap();
            if cond {
                machine.pointer = scenario.elements.partition_point(|p| p.0 < *target);
            } else {
                machine.pointer += 1;
            }
            StepResult::Continue
        }
        Element::Select { cond, params, first_line } => todo!(),
    }
}

struct Scenario {
    id: u32,
    elements: Vec<(usize, Element)>,
}

fn main() -> Result<(), MyError> {
    let path = std::env::args_os().nth(1).unwrap_or_else(|| {
        r"C:\Users\Host\Downloads\SEEN.txt".into()
    });

    let buffer = std::fs::read(path)?;
    let archive = read_archive(&buffer)?;
    let scenarios: Vec<_> = archive
        .scenarios()
        .map(|s| Scenario {
            id: s.id(),
            elements: s.read().unwrap(),
        })
        .collect();

    let mut m = Machine::new(scenarios.iter().next().unwrap().id);
    for _ in 0..500 {
        match step(&mut m, &scenarios) {
            StepResult::Continue | StepResult::Halt => {}
            StepResult::Call(meta, params) => {
                println!("calling {} with {:?}", meta.opcode, params);
            }
            StepResult::Exit => break,
            StepResult::Text(text) => {
                println!(">> {}", text);
            }
        }
    }

    Ok(())
}
