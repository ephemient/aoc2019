use super::util;
use std::char;
use std::error;
use std::iter::Sum;

pub fn part1<'a, I, S>(lines: I) -> Result<String, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut value = lines
        .into_iter()
        .nth(0)
        .ok_or(util::Error)?
        .as_ref()
        .chars()
        .filter_map(|c| c.to_digit(10))
        .map(|i| i as i32)
        .collect::<Vec<_>>();
    for _ in 0..100 {
        value = (0..value.len())
            .map(|i| {
                i32::sum(value.iter().enumerate().map(|(j, x)| {
                    let y = match (j + 1) % (4 * i + 4) / (i + 1) {
                        1 => *x,
                        3 => -x,
                        _ => 0,
                    };
                    y
                }))
                .abs()
                    % 10
            })
            .collect();
    }
    Ok(value
        .iter()
        .take(8)
        .filter_map(|i| char::from_digit(*i as u32, 10))
        .collect())
}

pub fn part2<'a, I, S>(lines: I) -> Result<String, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut value = lines
        .into_iter()
        .nth(0)
        .ok_or(util::Error)?
        .as_ref()
        .chars()
        .filter_map(|c| c.to_digit(10))
        .map(|i| i as i32)
        .collect::<Vec<_>>();
    let logical_len = 10000 * value.len();
    let offset = value
        .iter()
        .take(7)
        .fold(0, |acc, x| 10 * acc + *x as usize);
    assert!(offset < logical_len && logical_len <= 2 * offset);
    value = (offset..logical_len)
        .map(|i| value[i % value.len()])
        .collect();
    for _ in 0..100 {
        for i in (1..value.len()).rev() {
            value[i - 1] += value[i];
        }
        for i in value.iter_mut() {
            *i = i.abs() % 10;
        }
    }
    Ok(value
        .iter()
        .take(8)
        .filter_map(|i| char::from_digit(*i as u32, 10))
        .collect())
}
