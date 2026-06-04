use std::{env, fs};

pub mod day01;

fn main() {
    let mut args = env::args().skip(1);

    let day_str = args
        .next()
        .unwrap_or_else(|| exit_with_error("Missing day argument."));
    let input_file = args
        .next()
        .unwrap_or_else(|| exit_with_error("Missing input file argument."));

    let day: u8 = day_str
        .parse()
        .unwrap_or_else(|_| exit_with_error("Could not parse day argument as int"));

    if !fs::exists(&input_file).unwrap_or(false) {
        exit_with_error("Could not open file {input_file}");
    }

    match day {
        1 => day01::solve(&input_file),
        _ => exit_with_error("Day {day} is not implemented yet"),
    }
}

fn exit_with_error(error: &str) -> ! {
    eprintln!("{error}");
    eprintln!(
        "Usage:

    cargo run <day> <path/to/input>

For example:

    cargo run 02 input/02.txt
    "
    );
    std::process::exit(1)
}
