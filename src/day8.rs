use super::util;
use itertools::Itertools;
use std::collections::HashMap;
use std::error;

pub fn part1<'a, I, S>(
    lines: I,
    width: usize,
    height: usize,
) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let counts = line
        .chars()
        .chunks(width * height)
        .into_iter()
        .map(|chunk| {
            let mut counts: HashMap<char, usize> = HashMap::new();
            for c in chunk {
                *counts.entry(c).or_insert(0) += 1;
            }
            counts
        })
        .min_by_key(|counts| *counts.get(&'0').unwrap_or(&0usize))
        .ok_or(util::Error)?;
    let one = *counts.get(&'1').unwrap_or(&0usize);
    let two = *counts.get(&'2').unwrap_or(&0usize);
    return Ok(one * two);
}

pub fn part2<'a, I, S>(
    lines: I,
    width: usize,
    height: usize,
) -> Result<String, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let pixels = line
        .chars()
        .chunks(width * height)
        .into_iter()
        .fold(None, |acc: Option<Vec<char>>, s| match acc {
            None => Some(s.collect::<Vec<char>>()),
            Some(acc) => Some(
                acc.into_iter()
                    .zip(s)
                    .map(|(a, b)| match a {
                        '2' => b,
                        a => a,
                    })
                    .collect::<Vec<char>>(),
            ),
        })
        .ok_or(util::Error)?;
    return Ok(pixels
        .chunks(width)
        .into_iter()
        .map(|line| {
            line.into_iter()
                .map(|c| match *c {
                    '0' => '\u{2592}',
                    '1' => '\u{2593}',
                    '2' => '\u{2591}',
                    c => c,
                })
                .collect::<String>()
        })
        .join("\n"));
}
