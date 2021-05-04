use crate::archive::read_archive;
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

    let buffer = std::fs::read(path)?;
    let archive = read_archive(&buffer)?;

    for scenario in archive.scenarios() {
        let result = std::panic::catch_unwind(|| {
            let s = std::time::Instant::now();
            let items = scenario.read().unwrap();
            (s.elapsed(), items)
        });
        match result {
            Ok((time, items)) => {
                total += time;
                total_items.extend(items.into_iter().map(|(o, e)| (scenario.id, o, e)));
            }
            Err(e) => {
                println!("{:?}", e);
                panic!();
            }
        }
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
                println!("{}:{}  {:?} {:#?}", scenario, offset, cond, params)
            }
        }
    }

    Ok(())
}
