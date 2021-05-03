use std::io::Read;

use crate::archive::parse_seen;
use crate::parse::Element;

mod parse;
mod archive;
mod keys;

type MyError = Box<dyn std::error::Error>;


fn main() -> Result<(), MyError> {
    let path = std::env::args_os().nth(1).unwrap_or_else(|| {
        r"C:\Users\Host\Downloads\SEEN.txt".into()
    });

    let mut total = std::time::Duration::default();
    let mut total_items = Vec::new();

    let file = std::fs::File::open(path)?;
    let mut reader = std::io::BufReader::new(file);
    let mut buff = vec![];
    reader.read_to_end(&mut buff)?;
    let archive = parse_seen(&buff)?;

    for scenario in archive.scenarios() {
        let result = std::panic::catch_unwind(|| {
            let s = std::time::Instant::now();
            let items = scenario.read().unwrap();
            (s.elapsed(), items)
        });
        match result {
            Ok((time, items)) => {
                total += time;
                total_items.extend(items.into_iter());
            }
            Err(e) => {
                println!("{:?}", e);
                panic!();
            }
        }
    }

    dbg!(total, total_items.len());

    for (pos, item) in total_items.iter() {
        match item {
            Element::Halt => {}
            Element::Entrypoint(_) => {}
            Element::Kidoku(_) => {}
            Element::Line(_) => {}
            Element::Expr(_) => {
                // println!("{:?}", e);
            }
            Element::Bytecode(b) => {
                println!("{:?} @ {}", b, pos);
            }
            Element::Textout(_) => {
                // println!("{}", t);
            }
            Element::FunctionCall { .. } => {}
            Element::GoSubWith { target, .. } => {
                assert!(total_items.iter().any(|(pos, _)| pos == target));
            }
            Element::Goto { target, .. } => {
                assert!(total_items.iter().any(|(pos, _)| pos == target));
            }
            Element::GotoIf { target, .. } => {
                assert!(total_items.iter().any(|(pos, _)| pos == target));
            }
            Element::Select { .. } => {}
        }
    }

    Ok(())
}
