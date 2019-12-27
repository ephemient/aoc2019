use super::util;
use std::error::Error;
use std::iter;

fn fuel(mass: i32) -> Option<i32> {
    Some(mass / 3 - 2).filter(|weight| *weight > 0)
}

fn fuels(mass: i32) -> i32 {
    iter::successors(fuel(mass), |x| fuel(*x)).sum()
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(util::parse_many(lines)?
        .iter()
        .cloned()
        .filter_map(fuel)
        .sum())
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(util::parse_many(lines)?.iter().cloned().map(fuels).sum())
}
