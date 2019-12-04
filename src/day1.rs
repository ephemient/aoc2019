use super::util;
use std::error::Error;
use std::iter::{self, Sum};

fn fuel(mass: &i32) -> Option<i32> {
    Some(mass / 3 - 2).filter(|weight| *weight > 0)
}

fn fuels(mass: &i32) -> i32 {
    i32::sum(iter::successors(fuel(mass), fuel))
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(i32::sum(util::parse_many(lines)?.iter().filter_map(fuel)))
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(i32::sum(util::parse_many(lines)?.iter().map(fuels)))
}
