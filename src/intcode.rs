use std::error;
use std::fmt;
use std::ops::{Add, Mul};

#[derive(Clone, Debug)]
pub struct Error {
    msg: String,
}

impl Error {
    pub fn new(msg: String) -> Error {
        Error { msg }
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

pub trait Environment<T, E> {
    fn input(&mut self) -> Result<T, E>;
    fn output(&mut self, value: T) -> Result<(), E>;
}

impl<I, O, T, E> Environment<T, E> for (I, O)
where
    I: FnMut() -> Result<T, E>,
    O: FnMut(T) -> Result<(), E>,
{
    fn input(&mut self) -> Result<T, E> {
        self.0()
    }
    fn output(&mut self, value: T) -> Result<(), E> {
        self.1(value)
    }
}

pub struct Intcode<'a, T> {
    mem: &'a mut Vec<T>,
    ip: usize,
    base: isize,
}

impl<'a, T> Intcode<'a, T> {
    pub fn new(mem: &'a mut Vec<T>) -> Intcode<'a, T> {
        Intcode {
            mem,
            ip: 0,
            base: 0,
        }
    }
}

impl<'a, T> Intcode<'a, T>
where
    T: Clone + Default,
{
    fn get_raw(&self, idx: usize) -> T {
        match self.mem.get(idx) {
            Some(value) => value.clone(),
            None => T::default(),
        }
    }
}

impl<'a, T> Intcode<'a, T>
where
    T: Clone + Default + Into<i64>,
{
    fn arg(&mut self, n: u32) -> Result<T, Error> {
        let i = self.index(n)?;
        Ok(self.get_raw(i))
    }

    fn arg_mut(&mut self, n: u32) -> Result<&mut T, Error> {
        let idx = self.index(n)?;
        if idx >= self.mem.len() {
            self.mem.resize_with(idx + 1, T::default);
        }
        Ok(&mut self.mem[idx])
    }

    fn index(&mut self, n: u32) -> Result<usize, Error> {
        let op = self.get_raw(self.ip).into();
        match op / 10i64.pow(n + 1) % 10 {
            0 => Ok(self.get_raw(self.ip + n as usize).into() as usize),
            1 => Ok(self.ip + n as usize),
            2 => Ok((self.base + self.get_raw(self.ip + n as usize).into() as isize) as usize),
            mode => Err(Error::new(format!("bad mode {}", mode))),
        }
    }
}

impl<'a, T> Intcode<'a, T>
where
    T: Clone + Default + From<bool> + Into<i64> + Add<Output = T> + Mul<Output = T> + Ord,
{
    fn step<E>(&mut self, env: &mut impl Environment<T, E>) -> Result<bool, E>
    where
        E: From<Error>,
    {
        let op = self.get_raw(self.ip).into();
        match op % 100 {
            1 => {
                *self.arg_mut(3)? = self.arg(1)? + self.arg(2)?;
                self.ip += 4;
            }
            2 => {
                *self.arg_mut(3)? = self.arg(1)? * self.arg(2)?;
                self.ip += 4;
            }
            3 => {
                *self.arg_mut(1)? = env.input()?;
                self.ip += 2;
            }
            4 => {
                env.output(self.arg(1)?)?;
                self.ip += 2;
            }
            5 => {
                self.ip = if self.arg(1)? == T::default() {
                    self.ip + 3
                } else {
                    self.arg(2)?.into() as usize
                }
            }
            6 => {
                self.ip = if self.arg(1)? == T::default() {
                    self.arg(2)?.into() as usize
                } else {
                    self.ip + 3
                }
            }
            7 => {
                *self.arg_mut(3)? = T::from(self.arg(1)? < self.arg(2)?);
                self.ip += 4;
            }
            8 => {
                *self.arg_mut(3)? = T::from(self.arg(1)? == self.arg(2)?);
                self.ip += 4;
            }
            9 => {
                self.base += self.arg(1)?.into() as isize;
                self.ip += 2;
            }
            99 => return Ok(false),
            _ => return Err(Error::new(format!("bad opcode: {}", op)).into()),
        }
        Ok(true)
    }

    pub fn run<E>(&mut self, env: &mut impl Environment<T, E>) -> Result<(), E>
    where
        E: From<Error>,
    {
        while self.step(env)? {}
        Ok(())
    }
}
