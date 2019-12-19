use super::intcode::{self, Intcode};
use super::util;
use fallible_iterator::{self, FallibleIterator};
use std::error;
use std::ops::{Add, Mul};

fn bot<T>(mem: &mut Vec<T>, x: T, y: T) -> Result<bool, intcode::Error>
where
    T: Clone + Default + From<bool> + Into<i64> + Add<Output = T> + Mul<Output = T> + Ord,
{
    let input = vec![x, y];
    let mut input_iterator = input.iter().cloned();
    let mut output: Option<bool> = None;
    Intcode::new(mem).run(&mut (
        || {
            input_iterator
                .next()
                .ok_or_else(|| intcode::Error::new("no input".to_string()))
        },
        |value| -> Result<_, intcode::Error> {
            output = Some(value != T::default());
            Ok(())
        },
    ))?;
    output.ok_or_else(|| intcode::Error::new("no output".to_string()))
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mem: Vec<i64> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    Ok(
        fallible_iterator::convert((0i64..50).flat_map(|x| (0i64..50).map(move |y| Ok((x, y)))))
            .filter(|(x, y)| bot(&mut mem.clone(), *x, *y))
            .count()?,
    )
}

pub fn part2<'a, I, S>(lines: I) -> Result<i64, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mem: Vec<i64> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    let mut x = 0i64;
    let mut y = 99i64;
    loop {
        while !bot(&mut mem.clone(), x, y)? {
            x += 1;
        }
        if bot(&mut mem.clone(), x + 99, y - 99)? {
            return Ok(10000 * x + y - 99);
        }
        y += 1;
    }
}
