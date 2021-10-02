#![feature(slice_group_by)]

use std::borrow::Cow;
use std::cmp::Reverse;
use std::collections::HashMap;
use std::fmt::{Formatter, Write};

use log::{debug, trace, warn};

use reallive::{CallMeta, Element, Expr, MemoryBank, Operator, read_archive, parse_line};

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

    fn get(&self, bank: MemoryBank, location: usize) -> Value {
        match bank.0 {
            0..=6 => Value::Int(self.int_banks[bank.0 as usize][location]),
            7 | 25 => Value::Int(self.int_banks[7][location]), // Z
            8 | 11 => Value::Int(self.int_banks[8][location]), // L
            0x0a => Value::Str(self.str_banks[0][location].clone().into()), // K
            0x0c => Value::Str(self.str_banks[1][location].clone().into()), // M
            0x12 => Value::Str(self.str_banks[2][location].clone().into()), // S
            _ => todo!("getting {:?}[{}]", bank, location),
        }
    }

    fn set(&mut self, bank: MemoryBank, location: usize, value: Value) {
        match (bank.0, value) {
            (0..=6, Value::Int(x)) => {
                self.int_banks[bank.0 as usize][location] = x;
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

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum FrameType {
    Root,
    GoSub,
    FarCall,
}

#[derive(Debug)]
struct StackFrame {
    pointer: usize,
    scenario: u32,
    r#type: FrameType,
}

impl Machine {
    fn new(scenario: u32) -> Self {
        Self {
            memory: Memory::new(),
            call_stack: vec![
                StackFrame {
                    pointer: 0,
                    scenario,
                    r#type: FrameType::Root,
                }
            ],
        }
    }

    fn frame_mut(&mut self) -> Option<&mut StackFrame> {
        self.call_stack.last_mut()
    }

    fn push_frame(&mut self, frame: StackFrame) {
        debug!("push {:?}", &frame);
        self.call_stack.push(frame)
    }

    fn ret_with(&mut self, value: Option<Value>, frame_type: FrameType) {
        if let Some(value) = value {
            self.memory.store = value;
        }
        let frame = self.call_stack.pop().expect("call stack is empty");
        assert_eq!(frame.r#type, frame_type);
        debug!("pop {:?}", &frame);
    }
}

#[derive(Clone)]
enum Value {
    Str(Box<str>),
    Int(i32),
    Bool(bool),
    Tuple(Box<[Value]>),
    Unknown,
}

impl<'s> std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(s) => write!(f, "{:?}", s),
            Value::Int(s) => write!(f, "{}", s),
            Value::Bool(s) => write!(f, "{:?}", s),
            Value::Tuple(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    write!(f, "{:?}", item)?;
                    if i != items.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
            Value::Unknown => write!(f, "???"),
        }
    }
}

impl Value {
    fn as_str(&self) -> Option<&str> {
        match self {
            Value::Str(s) => Some(s),
            Value::Bool(_) | Value::Int(_) | Value::Tuple(_) | Value::Unknown => None,
        }
    }

    fn as_int(&self) -> Option<i32> {
        match self {
            Value::Str(_) | Value::Bool(_) | Value::Tuple(_) | Value::Unknown => None,
            Value::Int(s) => Some(*s),
        }
    }

    fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Str(_) | Value::Int(_) | Value::Tuple(_) | Value::Unknown => None,
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

fn evaluate_binary_expr(lhs: &Expr, op: Operator, rhs: &Expr, machine: &mut Machine) -> Value {
    if let Operator::Assign = op {
        let (bank, location) = lhs.as_memory_location().unwrap();
        let location = evaluate_expr(location, machine).as_int().unwrap() as usize;
        let value = evaluate_expr(rhs, machine);
        machine.memory.set(bank, location, value.clone());
        return value;
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
            evaluate_binary_expr(lhs, *op, rhs, machine)
        }
        Expr::Unary { op, expr } => {
            let value = evaluate_expr(expr, machine).as_int().unwrap();
            match op {
                Operator::Minus => Value::Int(-value),
                _ => todo!("{:?}", op),
            }
        }
        Expr::Special { .. } => Value::Unknown,
        Expr::Tuple { elements: items } => {
            let items: Vec<_> = items.iter()
                .map(|it| evaluate_expr(it, machine))
                .collect();
            Value::Tuple(items.into_boxed_slice())
        }
    }
}

fn evaluate_place(expr: &Expr, machine: &mut Machine) -> (MemoryBank, usize) {
    let (bank, location) = expr.as_memory_location().expect("lhs should a place in memory");
    let location = evaluate_expr(location, machine).as_int().expect("not a valid location");
    (bank, location as usize)
}

fn step<'s>(machine: &mut Machine, scenarios: &'s [Scenario]) -> StepResult<'s> {
    let mut frame = machine.frame_mut().unwrap();

    let idx = match scenarios.binary_search_by_key(&frame.scenario, |s| s.id) {
        Ok(idx) => idx,
        Err(_) => panic!("could not found a scenario: {}", frame.scenario),
    };

    let scenario = &scenarios[idx];
    let inst = match scenario.elements.get(frame.pointer) {
        Some((_, inst)) => inst,
        None => return StepResult::Exit,
    };

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
        Element::Expr(expr) => {
            trace!("{}@{}: EXECUTE {:?}", frame.pointer, scenario.id, expr);
            match expr {
                Expr::Binary { lhs, op: Operator::Assign, rhs } => {
                    let (bank, location) = evaluate_place(lhs, machine);
                    let result = evaluate_expr(rhs, machine);
                    machine.memory.set(bank, location as _, result);
                }
                Expr::Binary { lhs, .. } => {
                    let (bank, location) = evaluate_place(lhs, machine);
                    let result = evaluate_expr(expr, machine);
                    debug!("found top-level binary expression. treating lhs as a place to store a result: {:?} = {:?}", lhs, expr);
                    debug!("= {:?}", result);
                    machine.memory.set(bank, location as _, result);
                }
                _ => todo!("{:?}", expr),
            }

            machine.frame_mut().unwrap().pointer += 1;
            StepResult::Continue
        }
        Element::Textout(text) => {
            trace!("{}@{}: PRINT {:?}", frame.pointer, scenario.id, text);
            frame.pointer += 1;
            StepResult::Text(Cow::Borrowed(text))
        }
        Element::FunctionCall { meta, params } => {
            trace!("{}@{}: CALL {:?} {:?}", frame.pointer, scenario.id, &meta, &params);
            machine.frame_mut().unwrap().pointer += 1;
            StepResult::Call(meta, params)
        }
        Element::GoSubWith { target, params, meta } => {
            trace!("{}@{}: GOSUB_WITH {:?} {:?} -> {}", frame.pointer, scenario.id, meta, params, target);
            frame.pointer += 1;
            let scenario_id = frame.scenario;
            machine.push_frame(StackFrame {
                pointer: scenario.index_of_instr(*target),
                scenario: scenario_id,
                r#type: FrameType::GoSub,
            });
            StepResult::Continue
        }
        Element::Goto { target } => {
            trace!("{}@{}: GOTO {}", frame.pointer, scenario.id, target);
            frame.pointer = scenario.index_of_instr(*target);
            StepResult::Continue
        }
        Element::GotoIf { target, cond } => {
            trace!("{}@{}: GOTO_IF {:?} -> {}", frame.pointer, scenario.id, cond, target);
            let cond = evaluate_expr(cond, machine).as_bool().unwrap();
            if cond {
                machine.frame_mut().unwrap().pointer = scenario.index_of_instr(*target);
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

impl Scenario {
    fn index_of_instr(&self, offset: usize) -> usize {
        let index = self.elements.partition_point(|p| p.0 < offset);
        let nearest_offset = self.elements[index].0;
        if nearest_offset != offset {
            warn!("could not find index of the instruction at offset {} - using offset {} instead",
                  offset, nearest_offset);
        }
        debug!("instruction at offset {} in scenario {} has index {}", nearest_offset, self.id, index);
        index
    }
}

fn main() -> Result<(), MyError> {
    env_logger::init();
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

    let mut machine = Machine::new(9030);
    for _ in 0..1000 {
        match step(&mut machine, &scenarios) {
            StepResult::Continue | StepResult::Halt => {}
            StepResult::Call(meta, args) => call_function(&mut machine, meta, args, &scenarios),
            StepResult::Exit => break,
            StepResult::Text(text) => {
                debug!(">> {}", text);
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

struct Operand {
    value: Value,
    source: Option<(MemoryBank, u32)>,
}

impl std::fmt::Debug for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.value)?;
        if let Some((bank, location)) = self.source {
            write!(f, "~{:?}[{:?}]", bank, location)?;
        }
        Ok(())
    }
}

fn call_function(machine: &mut Machine, meta: &CallMeta, args: &[Expr], scenarios: &[Scenario]) {
    let evaluated: Vec<Operand> = args.iter().map(|expr| {
        let value = evaluate_expr(expr, machine);
        let source = if let Expr::MemRef { bank, location } = expr {
            let location = evaluate_expr(location, machine).as_int().unwrap() as u32;
            Some((*bank, location))
        } else {
            None
        };

        Operand { value, source }
    }).collect();

    match (meta.module_type, meta.module, meta.opcode, meta.overload) {
        (0, 1, 12, 1) => { // far_call(scenario, entrypoint)
            let scenario_id = evaluate_expr(&args[0], machine).as_int().unwrap();
            let entrypoint = evaluate_expr(&args[1], machine).as_int().unwrap();
            debug!("farcall({}, {})", scenario_id, entrypoint);

            let scenario = scenarios.iter().find(|s| s.id == scenario_id as _).unwrap();
            let target = scenario.elements.iter().find_map(|(offset, element)| match element {
                &Element::Entrypoint(ep) if ep == entrypoint as _ => Some(*offset),
                _ => None,
            }).unwrap();
            trace!("jumping to {}@{}", target, scenario_id);
            machine.push_frame(StackFrame {
                pointer: target,
                scenario: scenario.id,
                r#type: FrameType::FarCall,
            });
        }
        (0, 1, 13, 0) => { // rtl
            debug!("rtl");
            machine.ret_with(None, FrameType::FarCall);
        }
        (0, 1, 17, 1) => { // ret_with
            debug!("ret_with");
            machine.ret_with(None, FrameType::GoSub);
        }
        (0, 1, 19, 0) => { // rtl(val)
            let val = evaluate_expr(&args[0], machine);
            debug!("rtl({:?})", val);
            machine.ret_with(Some(val), FrameType::FarCall);
        }
        (0, 1, 19, 1) => { // rtl(val)
            debug!("rtl");
            machine.ret_with(None, FrameType::FarCall);
        }
        (0, 3, 17, 0) => { // pause
            debug!("pause: TODO");
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
            debug!("playing koe: {:?}", evaluated);
        }
        (1, 73, 3001, 0) => {
            debug!("menu_load: {:?}", evaluated);
        }
        (1, 81, 1050, 0) => {
            debug!("recLoad_0: {:?}", evaluated);
        }
        (1, 4, 110, 1) => {
            debug!("ResetTimer: {:?}", evaluated);
        }
        (1, 4, 114, 1) => {
            debug!("Timer: {:?}", evaluated);
        }
        (1, 81, 1049, 0) => {
            debug!("Rotate: {:?}", evaluated);
        }
        (1, 31, 0, 0) => {
            debug!("Refresh: {:?}", evaluated);
        }
        (1, 4, 2250, 0) => {
            debug!("SetAutoMode: {:?}", evaluated);
        }
        (1, 4, 2223, 0) => {
            debug!("SetMessageSpeed: {:?}", evaluated);
        }
        (1, 4, 2224, 0) => {
            debug!("SetMessageNoWait: {:?}", evaluated);
        }
        (1, 4, 2051, 0) => {
            debug!("SetSkipAnimations: {:?}", evaluated);
        }
        (1, 4, 2227, 0) => {
            debug!("SetBgmKoeFade: {:?}", evaluated);
        }
        (1, 4, 2226, 0) => {
            debug!("SetBgmKoeFadeVol: {:?}", evaluated);
        }
        (1, 4, 2057, 0) => {
            debug!("Unknown_2057: {:?}", evaluated);
        }
        (1, 4, 2056, 0) => {
            debug!("Unknown_2056: {:?}", evaluated);
        }
        (0, 1, 11, 1) => {
            debug!("jump: {:?}", evaluated);
            let frame = machine.call_stack.last_mut().unwrap();
            frame.scenario = evaluated[0].value.as_int().unwrap() as _;
            frame.pointer = evaluated[1].value.as_int().unwrap() as _;
        }
        (1, 4, 1212, 1) => {
            debug!("HideSyscom: {:?}", evaluated);
        }
        (1, 30, 22, 0) => {
            debug!("DrawManual: {:?}", evaluated);
        }
        (1, 4, 101, 0) => {
            debug!("waitC: {:?}", evaluated);
        }
        (1, 61, 10, 0) => {
            debug!("objChildFg: {:?}", evaluated);
        }
        (1, 71, 1000, 0) => {
            debug!("ObjFgCreation: {:?}", evaluated);
        }
        (1, 81, 1000, 0) => {
            debug!("Unknown_1000: {:?}", evaluated);
        }
        (1, 81, 1039, 0) => {
            debug!("objGetPattNo: {:?}", evaluated);
        }
        (1, 81, 1004, 0) => {
            debug!("Unknown_1004: {:?}", evaluated);
        }
        (1, 4, 130, 0) => {
            debug!("FlushClick: {:?}", evaluated);
        }
        (1, 4, 620, 0) => {
            debug!("InitExFrames: {:?}", evaluated);
        }
        (1, 4, 630, 0) => {
            debug!("ReadExFrames: {:?}", evaluated);
        }
        (1, 4, 133, 0) => {
            debug!("GetCursorPos: {:?}", evaluated);
        }
        (1, 4, 800, 0) => {
            debug!("index_series: {:?}", evaluated);
        }
        (1, 81, 1003, 0) => {
            debug!("Unknown_1003: {:?}", evaluated);
        }
        _ => {
            debug!("calling {:?} with args = {:?}", meta, evaluated);
        }
    }
}