use super::intcode::{self, Intcode};
use super::util;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::error;

struct Walk {
    grid: HashMap<(i32, i32), bool>,
    x: i32,
    y: i32,
    dx: i32,
    dy: i32,
    start: bool,
    moving: bool,
}

impl intcode::Environment<i64, intcode::Error> for Walk {
    fn input(&mut self) -> Result<i64, intcode::Error> {
        Ok(self
            .grid
            .get(&(self.x, self.y))
            .map_or_else(|| self.x == 0 && self.y == 0 && self.start, |b| *b) as i64)
    }
    fn output(&mut self, value: i64) -> Result<(), intcode::Error> {
        if self.moving {
            let (dx, dy) = match value {
                0 => (-self.dy, self.dx),
                _ => (self.dy, -self.dx),
            };
            self.x += dx;
            self.y += dy;
            self.dx = dx;
            self.dy = dy;
            self.moving = false;
        } else {
            self.grid.insert((self.x, self.y), value != 0);
            self.moving = true;
        }
        Ok(())
    }
}

fn walk(mem: &mut Vec<i64>, start: bool) -> Result<HashMap<(i32, i32), bool>, intcode::Error> {
    let mut walk = Walk {
        grid: HashMap::new(),
        x: 0,
        y: 0,
        dx: 0,
        dy: 1,
        start,
        moving: false,
    };
    Intcode::new(mem).run(&mut walk)?;
    Ok(walk.grid)
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i64> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    Ok(walk(&mut mem, false)?.len())
}

pub fn part2<'a, I, S>(lines: I) -> Result<String, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i64> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    let result = walk(&mut mem, true)?
        .into_iter()
        .filter(|(_, v)| *v)
        .map(|(k, _)| k)
        .collect::<HashSet<_>>();
    let (xs, ys) = result.clone().into_iter().unzip::<_, _, Vec<_>, Vec<_>>();
    let (min_x, max_x) = xs.into_iter().minmax().into_option().ok_or(util::Error)?;
    let (min_y, max_y) = ys.into_iter().minmax().into_option().ok_or(util::Error)?;
    Ok((min_y..=max_y)
        .rev()
        .map(|y| {
            (min_x..=max_x)
                .map(|x| {
                    if result.contains(&(x, y)) {
                        '\u{2593}'
                    } else {
                        '\u{2591}'
                    }
                })
                .collect::<String>()
        })
        .join("\n"))
}
