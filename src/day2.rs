use super::intcode::{Error, Intcode};
use super::util;
use std::error;

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i32> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    mem[1] = 12;
    mem[2] = 2;
    Intcode::new(&mut mem).run(&mut (
        || Err(Error::new("no input".to_string())),
        |_| Err(Error::new("no output".to_string())),
    ))?;
    Ok(mem[0])
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mem0: Vec<i32> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    for noun in 0..99 {
        for verb in 0..99 {
            let mut mem = mem0.clone();
            mem[1] = noun;
            mem[2] = verb;
            Intcode::new(&mut mem).run(&mut (
                || Err(Error::new("no input".to_string())),
                |_| Err(Error::new("no output".to_string())),
            ))?;
            if mem[0] == 19_69_07_20 {
                return Ok(100 * noun + verb);
            }
        }
    }
    Err(Box::new(util::Error))
}
