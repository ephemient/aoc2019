use super::intcode::{self, Intcode};
use super::util;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::error;

enum State {
    PendingX,
    PendingY,
    PendingOutput,
}

struct Draw {
    blocks: HashSet<(i64, i64)>,
    x: i64,
    y: i64,
    state: State,
}

impl intcode::Environment<i64, intcode::Error> for Draw {
    fn input(&mut self) -> Result<i64, intcode::Error> {
        Err(intcode::Error::new("no input".to_string()))
    }

    fn output(&mut self, value: i64) -> Result<(), intcode::Error> {
        match self.state {
            State::PendingX => {
                self.x = value;
                self.state = State::PendingY;
            }
            State::PendingY => {
                self.y = value;
                self.state = State::PendingOutput;
            }
            State::PendingOutput => {
                match value {
                    2 => self.blocks.insert((self.x, self.y)),
                    _ => self.blocks.remove(&(self.x, self.y)),
                };
                self.state = State::PendingX;
            }
        };
        Ok(())
    }
}

struct Bot {
    ball: Option<i64>,
    paddle: Option<i64>,
    score: Option<i64>,
    x: i64,
    y: i64,
    state: State,
}

impl intcode::Environment<i64, intcode::Error> for Bot {
    fn input(&mut self) -> Result<i64, intcode::Error> {
        match (self.ball, self.paddle) {
            (Some(ball), Some(paddle)) => match ball.cmp(&paddle) {
                Ordering::Less => Ok(-1),
                Ordering::Equal => Ok(0),
                Ordering::Greater => Ok(1),
            },
            _ => Err(intcode::Error::new("no display".to_string())),
        }
    }

    fn output(&mut self, value: i64) -> Result<(), intcode::Error> {
        match self.state {
            State::PendingX => {
                self.x = value;
                self.state = State::PendingY;
            }
            State::PendingY => {
                self.y = value;
                self.state = State::PendingOutput;
            }
            State::PendingOutput => {
                if self.x == -1 && self.y == 0 {
                    self.score = Some(value);
                } else if value == 3 {
                    self.paddle = Some(self.x);
                } else if value == 4 {
                    self.ball = Some(self.x);
                }
                self.state = State::PendingX;
            }
        };
        Ok(())
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i64> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    let mut draw = Draw {
        blocks: HashSet::new(),
        x: 0,
        y: 0,
        state: State::PendingX,
    };
    Intcode::new(&mut mem).run(&mut draw)?;
    Ok(draw.blocks.len())
}

pub fn part2<'a, I, S>(lines: I) -> Result<i64, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i64> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    mem[0] = 2;
    let mut bot = Bot {
        ball: None,
        paddle: None,
        score: None,
        x: 0,
        y: 0,
        state: State::PendingX,
    };
    Intcode::new(&mut mem).run(&mut bot)?;
    Ok(bot.score.ok_or(util::Error)?)
}
