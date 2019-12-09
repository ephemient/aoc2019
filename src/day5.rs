use super::intcode::{Error, Intcode};
use super::util;
use std::error;

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i32> =
        util::parse_many(&line.split(',').map(|s| s.clone()).collect::<Vec<&str>>())?;
    let mut output: Option<i32> = None;
    Intcode::new(&mut mem).run::<_, _, Error>(
        || Ok(1),
        |value| {
            output = Some(value);
            Ok(())
        },
    )?;
    return Ok(output.ok_or_else(|| Error::new("no output".to_string()))?);
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i32> =
        util::parse_many(&line.split(',').map(|s| s.clone()).collect::<Vec<&str>>())?;
    let mut output: Option<i32> = None;
    Intcode::new(&mut mem).run::<_, _, Error>(
        || Ok(5),
        |value| {
            output = Some(value);
            Ok(())
        },
    )?;
    return Ok(output.ok_or_else(|| Error::new("no output".to_string()))?);
}
