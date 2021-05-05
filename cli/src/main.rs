use std::cmp::Reverse;

use reallive::{Element, Expr, read_archive};

type MyError = Box<dyn std::error::Error>;

fn main() -> Result<(), MyError> {
    let path = std::env::args_os().nth(1).unwrap_or_else(|| {
        r"C:\Users\Host\Downloads\SEEN.txt".into()
    });

    let mut total = std::time::Duration::default();
    let mut total_items = Vec::new();

    let buffer = std::fs::read(path)?;
    let archive = read_archive(&buffer)?;

    let mut stats = Vec::new();

    for scenario in archive.scenarios() {
        let result = std::panic::catch_unwind(|| {
            let s = std::time::Instant::now();
            let items = scenario.read().unwrap();
            (s.elapsed(), items)
        });
        match result {
            Ok((time, items)) => {
                total += time;
                stats.push((scenario.id(), items.len(), time));
                total_items.extend(items.into_iter().map(|(o, e)| (scenario.id(), o, e)));
            }
            Err(e) => {
                println!("{:?}", e);
                panic!();
            }
        }
    }

    stats.sort_by_key(|k| Reverse(k.2 / k.1 as u32));
    for (scenario, items, time) in stats {
        println!("#{:04} | {:>5} items | {:?} | {:?}", scenario, items, time / items as u32, time);
    }

    println!("Parsed {} items in {:?}", total_items.len(), total);

    for (scenario, offset, item) in total_items.iter() {
        match item {
            Element::Halt => {}
            Element::Entrypoint(ep) => {
                // println!("{}", ep);
            }
            Element::Kidoku(_) => {}
            Element::Line(_) => {}
            Element::Expr(e) => {
                // println!("{:?}", e);
            }
            Element::Textout(_) => {
                // println!("{}", t);
            }
            Element::FunctionCall { meta, params } => {
                // for e in params {
                //     println!("{:?}", e);
                // }
                // println!("{:?} {:?}", meta, params);
            }
            Element::GoSubWith { target, .. } => {
                assert!(total_items.iter().any(|it| it.1 == *target));
            }
            Element::Goto { target, .. } => {
                assert!(total_items.iter().any(|it| it.1 == *target));
            }
            Element::GotoIf { target, .. } => {
                assert!(total_items.iter().any(|it| it.1 == *target));
            }
            Element::Select { cond, params, first_line } => {
                // println!("{}:{}  {:?} {:#?}", scenario, offset, cond, params)
            }
        }
    }
    let before = total_items.len();
    total_items.retain(|c| !matches!(c.2, Element::Line(_)));
    let removed = before - total_items.len();
    println!("{}", removed);

    drop(archive);
    drop(buffer);
    println!("{}", std::mem::size_of::<Element>());
    println!("{}", std::mem::size_of::<Expr>());
    total_items.shrink_to_fit();

    std::thread::sleep(std::time::Duration::from_secs(5));

    Ok(())
}
