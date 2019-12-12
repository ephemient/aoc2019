#[macro_use]
extern crate build_const;
#[macro_use]
extern crate lazy_static;

use std::collections::HashSet;
use std::env;
use std::io;
use std::iter::FromIterator;

mod day1;
mod day10;
mod day11;
mod day12;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod intcode;
mod util;

build_const!("aoc2019.rs");

fn ioerror() -> io::Error {
    util::to_ioerror(util::Error)
}

fn main() -> io::Result<()> {
    let args: HashSet<_> = HashSet::from_iter(env::args().skip(1));

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

    if args.is_empty() || args.contains("4") {
        println!("Day 4");
        println!("{:?}", day4::part1(DAY4).map_err(util::to_ioerror)?);
        println!("{:?}", day4::part2(DAY4).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("5") {
        println!("Day 5");
        println!("{:?}", day5::part1(DAY5).map_err(util::to_ioerror)?);
        println!("{:?}", day5::part2(DAY5).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("6") {
        println!("Day 6");
        println!("{:?}", day6::part1(DAY6).map_err(util::to_ioerror)?);
        println!("{:?}", day6::part2(DAY6).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("7") {
        println!("Day 7");
        println!("{:?}", day7::part1(DAY7).map_err(util::to_ioerror)?);
        println!("{:?}", day7::part2(DAY7).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("8") {
        println!("Day 8");
        println!("{:?}", day8::part1(DAY8, 25, 6).map_err(util::to_ioerror)?);
        println!("{}", day8::part2(DAY8, 25, 6).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("9") {
        println!("Day 9");
        println!("{:?}", day9::part1(DAY9).map_err(util::to_ioerror)?);
        println!("{:?}", day9::part2(DAY9).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("10") {
        println!("Day 10");
        println!("{:?}", day10::part1(DAY10));
        println!("{:?}", day10::part2(DAY10).ok_or_else(ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("11") {
        println!("Day 11");
        println!("{:?}", day11::part1(DAY11).map_err(util::to_ioerror)?);
        println!("{}", day11::part2(DAY11).map_err(util::to_ioerror)?);
        println!();
    }

    if args.is_empty() || args.contains("12") {
        println!("Day 12");
        println!("{:?}", day12::part1(DAY12).map_err(util::to_ioerror)?);
        println!("{:?}", day12::part2(DAY12).map_err(util::to_ioerror)?);
        println!();
    }

    Ok(())
}
