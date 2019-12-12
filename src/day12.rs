use super::util;
use regex::Regex;
use std::cmp::Ordering;
use std::error;
use std::iter::Sum;
use std::mem;
use std::ops::AddAssign;
use std::str::FromStr;

struct Simulation2<T> {
    points: Vec<T>,
    velocities: Vec<T>,
}

impl<T> Simulation2<T>
where
    T: Clone + Default,
{
    fn new(points: &[T]) -> Simulation2<T> {
        Simulation2 {
            points: points.to_vec(),
            velocities: points.iter().map(|_| T::default()).collect(),
        }
    }
}

impl<T> Iterator for Simulation2<T>
where
    T: Clone + Ord + From<i32> + AddAssign,
{
    type Item = Vec<(T, T)>;
    fn next(&mut self) -> Option<Self::Item> {
        for (point, velocity) in self.points.iter().zip(self.velocities.iter_mut()) {
            for other in self.points.iter() {
                match point.cmp(other) {
                    Ordering::Less => *velocity += T::from(1),
                    Ordering::Greater => *velocity += T::from(-1),
                    _ => {}
                }
            }
        }
        for (point, velocity) in self.points.iter_mut().zip(self.velocities.iter()) {
            *point += velocity.clone();
        }
        Some(
            self.points
                .iter()
                .cloned()
                .zip(self.velocities.iter().cloned())
                .collect(),
        )
    }
}

fn lcm(x: u64, y: u64) -> u64 {
    let mut a = x;
    let mut b = y;
    while a != 0 {
        b %= a;
        mem::swap(&mut a, &mut b);
    }
    x / b * y
}

fn parse2<'a, I, S, T>(lines: I) -> Result<Vec<Vec<T>>, <T as FromStr>::Err>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
    T: FromStr,
{
    lazy_static! {
        static ref RE: Regex = Regex::new(r#"-?\d+"#).unwrap();
    }
    let mut result: Vec<Vec<T>> = Vec::new();
    for line in lines.into_iter() {
        for (i, m) in RE.find_iter(line.as_ref()).enumerate() {
            let value = m.as_str().parse::<T>()?;
            match result.get_mut(i) {
                Some(v) => v.push(value),
                None => result.push(vec![value]),
            };
        }
    }
    Ok(result)
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(util::transpose(
        parse2::<_, _, i32>(lines)?
            .iter()
            .filter_map(|axis| Simulation2::new(axis).nth(999)),
    )
    .map(|axes| {
        i32::sum(axes.iter().map(|(p, _)| p.abs())) * i32::sum(axes.iter().map(|(_, v)| v.abs()))
    })
    .sum())
}

pub fn part2<'a, I, S>(lines: I) -> Result<u64, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(parse2::<_, _, i32>(lines)?.iter().fold(1, |acc, points| {
        lcm(
            acc,
            Simulation2::new(points)
                .position(|state| {
                    state
                        .iter()
                        .enumerate()
                        .all(|(i, (p, v))| *v == 0 && Some(*p) == points.get(i).cloned())
                })
                .unwrap() as u64
                + 1,
        )
    }))
}
