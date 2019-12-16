use super::util;
use std::char;
use std::cmp;
use std::error;

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
        for x in 0..value.len() {
            let mut sign = true;
            value[x] = (x..value.len())
                .step_by(2 * x + 2)
                .map(|base| {
                    let span_sum: i32 = value[base..cmp::min(base + x + 1, value.len())]
                        .iter()
                        .sum();
                    if sign {
                        sign = false;
                        -span_sum
                    } else {
                        sign = true;
                        span_sum
                    }
                })
                .sum::<i32>()
                .abs()
                % 10;
        }
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
    let offset = value
        .iter()
        .take(7)
        .fold(0, |acc, x| 10 * acc + *x as usize);
    let n = 10000 * value.len() - offset;
    assert!(8 <= n && n < offset);
    value = value
        .iter()
        .cloned()
        .cycle()
        .skip(offset % value.len())
        .take(n)
        .collect();
    for _ in 0..100 {
        let mut acc = 0;
        for i in (0..value.len()).rev() {
            acc = (acc + value[i]).abs() % 10;
            value[i] = acc;
        }
    }
    Ok(value
        .iter()
        .take(8)
        .filter_map(|i| char::from_digit(*i as u32, 10))
        .collect())
}
