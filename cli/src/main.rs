#![feature(array_map)]

use std::cmp::Reverse;

use reallive::{Element, Expr, read_archive, CallMeta, Operator};
use std::collections::HashMap;
use std::borrow::Cow;
use std::process::exit;

type MyError = Box<dyn std::error::Error>;

struct Memory {
    banks: [Vec<i32>; 6],
    register: i32,
}

impl Memory {
    fn new() -> Self {
        Self {
            banks: [(); 6].map(|_| vec![0; 10000]),
            register: 0,
        }
    }

    fn get(&self, bank: u8, location: usize) -> Value<'static> {
        Value::Integer(2137)
    }

    fn set(&mut self, bank: u8, location: usize, value: Value) {
        match (bank, value) {
            (0..=6, Value::Integer(x)) => {
                self.banks[bank as usize][location] = x;
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

#[derive(Debug)]
#[derive(Clone)]
enum Value<'s> {
    String(Cow<'s, str>),
    Integer(i32),
}

impl<'s> Value<'s> {
    fn as_integer(&self) -> Option<i32> {
        match self {
            Value::String(_) => None,
            Value::Integer(s) => Some(*s),
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
        Expr::IntConst { value } => Value::Integer(*value),
        Expr::MemRef { bank, location } => {
            let location = evaluate_expr(location, machine).as_integer().unwrap() as usize;
            machine.memory.get(*bank, location)
        }
        Expr::Binary { op, lhs, rhs } => {
            if let Operator::Assign = op {
                match &**lhs {
                    Expr::MemRef { bank, location } => {
                        let location = evaluate_expr(location, machine).as_integer().unwrap() as usize;
                        let value = evaluate_expr(rhs, machine);
                        machine.memory.set(*bank, location, value.clone());
                        return value;
                    }
                    _ => todo!(),
                }
            } else {
                let lhs = evaluate_expr(lhs, machine).as_integer().unwrap();
                let rhs = evaluate_expr(rhs, machine).as_integer().unwrap();
                match op {
                    Operator::Asterisk => Value::Integer(lhs * rhs),
                    Operator::Plus => Value::Integer(lhs + rhs),
                    Operator::Minus => Value::Integer(lhs - rhs),
                    Operator::Slash => Value::Integer(lhs / rhs),
                    _ => todo!("{:?}", op),
                }
            }
        }
        Expr::StoreRegister => {
            Value::Integer(machine.memory.register)
        }
        Expr::StringConst { value } => {
            Value::String(Cow::Borrowed(value))
        }
        Expr::Unary { op, expr } => {
            let value = evaluate_expr(expr, machine).as_integer().unwrap();
            match op {
                Operator::Minus => Value::Integer(-value),
                _ => todo!("{:?}", op),
            }
        }
        Expr::Special { .. } => Value::String("".into()),
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
            println!("goto {} if {:?}", target, cond);
            println!("evalauting as false");
            machine.pointer += 1;
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
    loop {
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
