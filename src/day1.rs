use super::util;
use std::error::Error;
use std::iter::{self, Sum};

fn fuel(mass: &i32) -> i32 {
    mass / 3 - 2
}

fn fuels(mass: &i32) -> i32 {
    fn refuel(mass: &i32) -> Option<i32> {
        Some(fuel(mass)).filter(|mass| mass > &0)
    }
    return i32::sum(iter::successors(refuel(mass), refuel));
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Result::Ok(i32::sum(util::parse_many(lines)?.iter().map(fuel)))
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Result::Ok(i32::sum(util::parse_many(lines)?.iter().map(fuels)))
}
