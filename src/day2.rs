use super::util;
use std::error;
use std::num::ParseIntError;

fn step(mem: &mut [i32], ip: &mut usize) -> Result<bool, util::Error> {
    match mem[*ip] {
        1 => {
            mem[mem[*ip + 3] as usize] = mem[mem[*ip + 1] as usize] + mem[mem[*ip + 2] as usize];
            *ip += 4;
        }
        2 => {
            mem[mem[*ip + 3] as usize] = mem[mem[*ip + 1] as usize] * mem[mem[*ip + 2] as usize];
            *ip += 4;
        }
        99 => return Ok(false),
        _ => return Err(util::Error),
    }
    return Ok(true);
}

fn run(mem: &mut [i32]) -> Result<(), util::Error> {
    let mut ip: usize = 0;
    while step(mem, &mut ip)? {}
    return Ok(());
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem = line
        .split(',')
        .map(|s| s.parse::<i32>())
        .collect::<Result<Vec<i32>, ParseIntError>>()?;
    mem[1] = 12;
    mem[2] = 2;
    run(&mut mem)?;
    return Ok(mem[0]);
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mem0 = line
        .split(',')
        .map(|s| s.parse::<i32>())
        .collect::<Result<Vec<i32>, ParseIntError>>()?;
    for noun in 0..99 {
        for verb in 0..99 {
            let mut mem = mem0.clone();
            mem[1] = noun;
            mem[2] = verb;
            run(&mut mem)?;
            if mem[0] == 19690720 {
                return Ok(100 * noun + verb);
            }
        }
    }
    return Err(Box::new(util::Error));
}
