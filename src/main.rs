#[macro_use]
extern crate build_const;

use std::collections::HashSet;
use std::env;
use std::io;
use std::iter::FromIterator;

mod day1;
mod day2;
mod day3;
mod util;

build_const!("aoc2019.rs");

fn ioerror() -> io::Error {
    util::to_ioerror(util::Error)
}

fn main() -> io::Result<()> {
    let args: HashSet<String> = HashSet::from_iter(env::args().skip(1));

    if args.is_empty() || args.contains("1") {
        println!("Day 1");
        println!("{:?}", day1::part1(DAY1).map_err(util::to_ioerror)?);
        println!("{:?}", day1::part2(DAY1).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("2") {
        println!("Day 2");
        println!("{:?}", day2::part1(DAY2).map_err(util::to_ioerror)?);
        println!("{:?}", day2::part2(DAY2).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("3") {
        println!("Day 3");
        println!("{:?}", day3::part1(DAY3).ok_or_else(ioerror)?);
        println!("{:?}", day3::part2(DAY3).ok_or_else(ioerror)?);
        println!();
    }

    return Ok(());
}
