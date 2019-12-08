use super::util;
use std::error;
use std::fmt;

#[derive(Clone, Debug)]
pub struct Error {
    msg: String,
}

impl Error {
    pub fn new(msg: String) -> Error {
        Error { msg: msg }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error: {}", self.msg)
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

fn step<I, O, E>(mem: &mut [i32], ip: &mut usize, input: &mut I, output: &mut O) -> Result<bool, E>
where
    I: FnMut() -> Result<i32, E>,
    O: FnMut(i32) -> Result<(), E>,
    E: From<Error>,
{
    let op = mem[*ip];
    let arg = |n: u32| -> usize {
        match op / 10i32.pow(n + 1) % 10 {
            0 => mem[*ip + n as usize] as usize,
            _ => *ip + n as usize,
        }
    };
    match op % 100 {
        1 => {
            mem[arg(3)] = mem[arg(1)] + mem[arg(2)];
            *ip += 4;
        }
        2 => {
            mem[arg(3)] = mem[arg(1)] * mem[arg(2)];
            *ip += 4;
        }
        3 => {
            mem[arg(1)] = input()?;
            *ip += 2;
        }
        4 => {
            output(mem[arg(1)])?;
            *ip += 2;
        }
        5 => {
            *ip = match mem[arg(1)] {
                0 => *ip + 3,
                _ => mem[arg(2)] as usize,
            }
        }
        6 => {
            *ip = match mem[arg(1)] {
                0 => mem[arg(2)] as usize,
                _ => *ip + 3,
            }
        }
        7 => {
            mem[arg(3)] = i32::from(mem[arg(1)] < mem[arg(2)]);
            *ip = *ip + 4;
        }
        8 => {
            mem[arg(3)] = i32::from(mem[arg(1)] == mem[arg(2)]);
            *ip = *ip + 4;
        }
        99 => return Ok(false),
        _ => return Err(Error::new(format!("bad opcode: {}", op)).into()),
    }
    return Ok(true);
}

pub fn run<I, O, E>(mem: &mut [i32], mut input: I, mut output: O) -> Result<(), E>
where
    I: FnMut() -> Result<i32, E>,
    O: FnMut(i32) -> Result<(), E>,
    E: From<Error>,
{
    let mut ip: usize = 0;
    while step(mem, &mut ip, &mut input, &mut output)? {}
    return Ok(());
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i32> =
        util::parse_many(&line.split(',').map(|s| s.clone()).collect::<Vec<&str>>())?;
    let mut output: Option<i32> = None;
    run::<_, _, Error>(
        &mut mem,
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
    run::<_, _, Error>(
        &mut mem,
        || Ok(5),
        |value| {
            output = Some(value);
            Ok(())
        },
    )?;
    return Ok(output.ok_or_else(|| Error::new("no output".to_string()))?);
}
