use super::util;
use regex::Regex;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::error;
use std::mem;
use std::ops::AddAssign;
use std::str::FromStr;

#[derive(Clone, PartialEq, Eq, Hash)]
struct Vector<T> {
    x: T,
    y: T,
    z: T,
}

impl<T> Vector<T>
where
    T: From<i32> + Ord,
{
    fn sign_to(&self, other: &Self) -> Self {
        Vector {
            x: T::from(cmp_signum(&self.x, &other.x)),
            y: T::from(cmp_signum(&self.y, &other.y)),
            z: T::from(cmp_signum(&self.z, &other.z)),
        }
    }
}

impl<T> AddAssign for Vector<T>
where
    T: AddAssign,
{
    fn add_assign(&mut self, other: Self) {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;
    }
}

struct Simulation<T> {
    points: Vec<Vector<T>>,
    velocities: Vec<Vector<T>>,
}

impl<T> Simulation<T>
where
    T: Clone + From<i32>,
{
    fn new(points: Vec<Vector<T>>) -> Simulation<T> {
        let velocities = points
            .iter()
            .map(|_| Vector {
                x: T::from(0),
                y: T::from(0),
                z: T::from(0),
            })
            .collect();
        return Simulation {
            points: points,
            velocities: velocities,
        };
    }
}

impl<T> Iterator for Simulation<T>
where
    T: AddAssign + Clone + From<i32> + Ord,
{
    type Item = Vec<(Vector<T>, Vector<T>)>;

    fn next(&mut self) -> Option<Self::Item> {
        for (i, p1) in self.points.iter().enumerate() {
            let velocity = self.velocities.get_mut(i).unwrap();
            for p2 in self.points.iter() {
                *velocity += p2.sign_to(p1);
            }
        }
        for (point, velocity) in self.points.iter_mut().zip(self.velocities.iter()) {
            *point += velocity.clone();
        }
        return Some(
            self.points
                .iter()
                .zip(self.velocities.iter())
                .map(|(point, velocity)| (point.clone(), velocity.clone()))
                .collect(),
        );
    }
}

fn cmp_signum<T>(one: &T, two: &T) -> i32
where
    T: Ord,
{
    match one.cmp(two) {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

fn lcm(x: u64, y: u64) -> u64 {
    let mut a = x;
    let mut b = y;
    while a != 0 {
        b %= a;
        mem::swap(&mut a, &mut b);
    }
    return x / b * y;
}

fn parse<'a, I, S, T>(lines: I) -> Result<Vec<Vector<T>>, <T as FromStr>::Err>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
    T: FromStr,
{
    lazy_static! {
        static ref RE: Regex = Regex::new(r#"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>"#).unwrap();
    }
    return Ok(lines
        .into_iter()
        .filter_map(|line| {
            RE.captures(line.as_ref())
                .and_then(|caps| Some((caps.get(1)?, caps.get(2)?, caps.get(3)?)))
                .map(|(x, y, z)| {
                    Ok(Vector {
                        x: x.as_str().parse::<T>()?,
                        y: y.as_str().parse::<T>()?,
                        z: z.as_str().parse::<T>()?,
                    })
                })
        })
        .collect::<Result<Vec<_>, _>>()?);
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let points: Vec<Vector<i32>> = parse(lines)?;
    let state = Simulation::new(points).nth(999).ok_or(util::Error)?;
    return Ok(state
        .into_iter()
        .map(|(point, velocity)| {
            (point.x.abs() + point.y.abs() + point.z.abs())
                * (velocity.x.abs() + velocity.y.abs() + velocity.z.abs())
        })
        .sum());
}

pub fn part2<'a, I, S>(lines: I) -> Result<u64, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let points: Vec<Vector<i32>> = parse(lines)?;
    let mut seen_x: HashSet<Vec<(i32, i32)>> = HashSet::new();
    let mut seen_y: HashSet<Vec<(i32, i32)>> = HashSet::new();
    let mut seen_z: HashSet<Vec<(i32, i32)>> = HashSet::new();
    for state in Simulation::new(points) {
        let state_x = state.iter().map(|(p, v)| (p.x, v.x)).collect::<Vec<_>>();
        let state_y = state.iter().map(|(p, v)| (p.y, v.y)).collect::<Vec<_>>();
        let state_z = state.iter().map(|(p, v)| (p.z, v.z)).collect::<Vec<_>>();
        if seen_x.replace(state_x).is_some()
            && seen_y.replace(state_y).is_some()
            && seen_z.replace(state_z).is_some()
        {
            break;
        }
    }
    return Ok(lcm(
        seen_x.len() as u64,
        lcm(seen_y.len() as u64, seen_z.len() as u64),
    ));
}
