#![feature(array_map)]

use std::borrow::Cow;
use std::cmp::Reverse;
use std::collections::HashMap;
use std::fmt::Formatter;

use reallive::{CallMeta, Element, Expr, Operator, read_archive};

type MyError = Box<dyn std::error::Error>;

struct Memory {
    int_banks: [Vec<i32>; 9],
    str_banks: [Vec<String>; 3],
    store: Value,
}

impl Memory {
    fn new() -> Self {
        Self {
            int_banks: [(); 9].map(|_| vec![0; 10000]),
            str_banks: [(); 3].map(|_| vec![String::new(); 10000]),
            store: Value::Int(0),
        }
    }

    fn get(&self, bank: u8, location: usize) -> Value {
        match bank {
            0..=6 => Value::Int(self.int_banks[bank as usize][location]),
            7 | 25 => Value::Int(self.int_banks[7][location]), // Z
            8 | 11 => Value::Int(self.int_banks[8][location]), // L
            0x0a => Value::Str(self.str_banks[0][location].clone().into()), // K
            0x0c => Value::Str(self.str_banks[1][location].clone().into()), // M
            0x12 => Value::Str(self.str_banks[2][location].clone().into()), // S
            _ => todo!("getting {}[{}]", bank, location),
        }
    }

    fn set(&mut self, bank: u8, location: usize, value: Value) {
        match (bank, value) {
            (0..=6, Value::Int(x)) => {
                self.int_banks[bank as usize][location] = x;
            }
            (7 | 25, Value::Int(x)) => {
                self.int_banks[7][location] = x;
            }
            (8 | 11, Value::Int(x)) => {
                self.int_banks[8][location] = x;
            }
            (0x0a, Value::Str(x)) => { // K
                self.str_banks[0][location] = x.to_string();
            }
            (0x0c, Value::Str(x)) => { // M
                self.str_banks[1][location] = x.to_string();
            }
            (0x12, Value::Str(x)) => { // S
                self.str_banks[2][location] = x.to_string();
            }
            (bank, value) => todo!("setting {}[{}] to {:?}", bank, location, value),
        }
    }
}

struct Machine {
    memory: Memory,
    call_stack: Vec<StackFrame>,
}

#[derive(Debug)]
struct StackFrame {
    pointer: usize,
    scenario: u32,
}

impl Machine {
    fn new(scenario: u32) -> Self {
        Self {
            memory: Memory::new(),
            call_stack: vec![
                StackFrame {
                    pointer: 0,
                    scenario,
                }
            ],
        }
    }

    fn frame_mut(&mut self) -> Option<&mut StackFrame> {
        self.call_stack.last_mut()
    }

    fn push_frame(&mut self, frame: StackFrame) {
        self.call_stack.push(frame)
    }

    fn ret_with(&mut self, val: Option<Value>) {
        if let Some(val) = val {
            self.memory.store = val;
        }
        self.call_stack.pop();
    }

    fn frames(&self) -> usize {
        self.call_stack.len()
    }
}

#[derive(Clone)]
enum Value {
    Str(Box<str>),
    Int(i32),
    Bool(bool),
}

impl<'s> std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Int(s) => write!(f, "{}", s),
            Value::Bool(s) => write!(f, "{:?}", s),
        }
    }
}

impl Value {
    fn as_str(&self) -> Option<&str> {
        match self {
            Value::Str(s) => Some(s),
            Value::Bool(_) | Value::Int(_) => None,
        }
    }

    fn as_int(&self) -> Option<i32> {
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
    Call(&'bc CallMeta, &'bc [Expr]),
    Exit,
}

fn evaluate_expr(expr: &Expr, machine: &mut Machine) -> Value {
    match expr {
        Expr::StoreRegister => machine.memory.store.clone(),
        Expr::StringConst { value } => Value::Str(value.clone()),
        Expr::IntConst { value } => Value::Int(*value),
        Expr::MemRef { bank, location } => {
            let location = evaluate_expr(location, machine).as_int().unwrap() as usize;
            machine.memory.get(*bank, location)
        }
        Expr::Binary { op, lhs, rhs } => {
            if let Operator::Assign = op {
                match lhs.as_ref() {
                    Expr::MemRef { bank, location } => {
                        let location = evaluate_expr(location, machine).as_int().unwrap() as usize;
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
            let value = evaluate_expr(expr, machine).as_int().unwrap();
            match op {
                Operator::Minus => Value::Int(-value),
                _ => todo!("{:?}", op),
            }
        }
        Expr::Special { .. } => Value::Str("/*SPECIAL*/".into()),
        Expr::Unknown { .. } => Value::Str("/*UNKNOWN*/".into()),
    }
}

fn step<'s>(machine: &mut Machine, scenarios: &'s [Scenario]) -> StepResult<'s> {
    let mut frame = machine.frame_mut().unwrap();

    let idx = match scenarios.binary_search_by_key(&frame.scenario, |s| s.id) {
        Ok(idx) => idx,
        Err(_) => panic!("could not found a scenario: {}", frame.scenario),
    };

    let scenario = &scenarios[idx];
    let inst = match scenario.elements.get(frame.pointer) {
        Some(x) => &x.1,
        None => return StepResult::Exit,
    };

    // println!("{:?}", inst);
    match inst {
        Element::Halt => {
            frame.pointer += 1;
            StepResult::Halt
        }
        Element::Entrypoint(_) => {
            frame.pointer += 1;
            StepResult::Continue
        }
        Element::Kidoku(_) => {
            frame.pointer += 1;
            StepResult::Continue
        }
        Element::Line(_) => {
            frame.pointer += 1;
            StepResult::Continue
        }
        Element::Expr(e) => {
            evaluate_expr(e, machine);
            machine.frame_mut().unwrap().pointer += 1;
            StepResult::Continue
        }
        Element::Textout(text) => {
            frame.pointer += 1;
            StepResult::Text(Cow::Borrowed(text))
        }
        Element::FunctionCall { meta, params } => {
            machine.frame_mut().unwrap().pointer += 1;
            StepResult::Call(meta, params)
        }
        Element::GoSubWith { target, params, meta } => {
            frame.pointer += 1;
            let scenario_id = frame.scenario;
            machine.push_frame(StackFrame {
                pointer: scenario.elements.partition_point(|p| p.0 < *target),
                scenario: scenario_id,
            });
            println!("{} {:?} {:?} {}", target, params, meta, machine.frames());
            StepResult::Continue
        }
        Element::Goto { target } => {
            frame.pointer = scenario.elements.partition_point(|p| p.0 < *target);
            StepResult::Continue
        }
        Element::GotoIf { target, cond } => {
            let cond = evaluate_expr(cond, machine).as_bool().unwrap();
            if cond {
                machine.frame_mut().unwrap().pointer = scenario.elements.partition_point(|p| p.0 < *target);
            } else {
                machine.frame_mut().unwrap().pointer += 1;
            }
            StepResult::Continue
        }
        Element::Select { .. } => todo!(),
        Element::GotoCase { .. } => todo!(),
        Element::Unknown0x0002000a { .. } => todo!(),
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

    let mut machine = Machine::new(scenarios.iter().next().unwrap().id);
    for _ in 0..1000 {
        match step(&mut machine, &scenarios) {
            StepResult::Continue | StepResult::Halt => {}
            StepResult::Call(meta, args) => call_function(&mut machine, meta, args),
            StepResult::Exit => break,
            StepResult::Text(text) => {
                println!(">> {}", text);
            }
        }
    }

    Ok(())
}

#[allow(unused)]
fn dump_used_functions(elements: &[Element]) {
    let mut unique: HashMap<_, usize> = HashMap::new();
    for element in elements {
        if let Element::FunctionCall { meta, params: _ } = element {
            *unique.entry((meta.module_type, meta.module, meta.opcode, meta.overload)).or_default() += 1;
        }
    }

    println!("Total function calls: {}", unique.values().copied().sum::<usize>());
    println!("Unique function calls: {}", unique.len());
    let mut unique: Vec<_> = unique.into_iter().collect();
    unique.sort_by_key(|&(_, count)| Reverse(count));
    for (x, num) in unique {
        println!("{}:{}:{}:{} -> {}", x.0, x.1, x.2, x.3, num);
    }
}

fn call_function(machine: &mut Machine, meta: &CallMeta, args: &[Expr]) {
    let evaluated = args.iter().map(|it| (it, evaluate_expr(it, machine))).collect::<Vec<_>>();
    match (meta.module_type, meta.module, meta.opcode, meta.overload) {
        (0, 1, 17, 1) => { // ret_with()
            machine.ret_with(None);
        }
        (0, 1, 18, 0) => { // far_call(scenario, entrypoint)
            let scenario = evaluate_expr(&args[0], machine).as_int().unwrap();
            let _entrypoint = evaluate_expr(&args[1], machine).as_int().unwrap();
            machine.push_frame(StackFrame {
                pointer: 0,
                scenario: scenario as u32,
            })
        }
        (0, 1, 19, 0) => { // rtl(val)
            let val = evaluate_expr(&args[0], machine);
            machine.ret_with(Some(val));
        }
        (0, 1, 19, 1) => { // rtl(val)
            machine.ret_with(None);
        }
        (0, 3, 17, 0) => { // pause
            // TODO
        }
        (1, 10, 0, 0) | // hantozen
        (1, 10, 2, 0) => { // strcat(dst, src)
            let src = evaluate_expr(&args[1], machine);
            match &args[0] {
                Expr::MemRef { bank, location } => {
                    let location = evaluate_expr(location, machine).as_int().unwrap();
                    machine.memory.set(*bank, location as usize, src);
                }
                _ => unimplemented!(),
            }
        }
        (1, 10, 4, 0) => { // strcmp(lhs, rhs)
            let lhs = evaluate_expr(&args[0], machine);
            let rhs = evaluate_expr(&args[1], machine);
            let result = lhs.as_str() == rhs.as_str();
            machine.memory.store = Value::Int(result as i32);
        }
        (1, 10, 5, 1) => { // strsub(dst, src, offset, length)
            let src = evaluate_expr(&args[1], machine);
            let src = src.as_str().unwrap();
            let offset = evaluate_expr(&args[2], machine).as_int().unwrap() as usize;
            let length = evaluate_expr(&args[3], machine).as_int().unwrap() as usize;
            let result = src.chars().skip(offset).take(length).collect::<String>().into_boxed_str();
            match &args[0] {
                Expr::MemRef { bank, location } => {
                    let location = evaluate_expr(location, machine).as_int().unwrap();
                    machine.memory.set(*bank, location as usize, Value::Str(result));
                }
                _ => unimplemented!(),
            }
        }
        (1, 10, 18, 0) => { // atoi(src)
            let src = evaluate_expr(&args[0], machine);
            let result = src.as_str().unwrap().trim().parse().unwrap_or(0);
            machine.memory.store = Value::Int(result);
        }
        (1, 23, 0, 1) => { // play
            println!("playing koe: {:?}", evaluated);
        }
        _ => {
            println!("calling {:?} with args = {:?}", meta, evaluated);
        }
    }
}