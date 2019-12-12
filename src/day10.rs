use super::util;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::mem;

fn parse<'a, I, S>(lines: I) -> Vec<(i32, i32)>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut result: Vec<(i32, i32)> = Vec::new();
    for (y, line) in lines.into_iter().enumerate() {
        for (x, c) in line.as_ref().chars().enumerate() {
            if c == '#' {
                result.push((x as i32, y as i32));
            }
        }
    }
    result
}

#[derive(Eq, PartialEq)]
struct Direction {
    negate: bool,
    slope: Option<(i32, i32)>,
}

impl Direction {
    fn between((x, y): (i32, i32), (u, v): (i32, i32)) -> Direction {
        if x == u {
            Direction {
                negate: y < v,
                slope: None,
            }
        } else {
            let sign = (x - u).signum();
            let mut a = (x - u).abs();
            let mut b = (y - v).abs();
            while a != 0 {
                b %= a;
                mem::swap(&mut a, &mut b);
            }
            Direction {
                negate: x > u,
                slope: Some(((x - u) / b * sign, (y - v) / b * sign)),
            }
        }
    }
}

impl Ord for Direction {
    fn cmp(&self, other: &Self) -> Ordering {
        self.negate
            .cmp(&other.negate)
            .then_with(|| match self.slope {
                None => match other.slope {
                    None => Ordering::Equal,
                    _ => Ordering::Less,
                },
                Some((x, y)) => match other.slope {
                    None => Ordering::Greater,
                    Some((u, v)) => (y * u).cmp(&(v * x)),
                },
            })
    }
}

impl PartialOrd for Direction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn explore(src: (i32, i32), points: &[(i32, i32)]) -> BTreeMap<Direction, Vec<(i32, i32)>> {
    let mut result: BTreeMap<Direction, Vec<(i32, i32)>> = BTreeMap::new();
    for dst in points {
        if src != *dst {
            result
                .entry(Direction::between(src, *dst))
                .or_insert_with(Vec::new)
                .push(*dst);
        }
    }
    result
}

fn best(points: Vec<(i32, i32)>) -> BTreeMap<Direction, Vec<(i32, i32)>> {
    let slice = &points[..];
    slice
        .iter()
        .map(|src| (*src, explore(*src, slice)))
        .max_by_key(|(_, result)| result.len())
        .map(|((x, y), mut result)| {
            for group in result.values_mut() {
                group.sort_by_key(|(u, v)| (x - u).abs() + (y - v).abs());
            }
            result
        })
        .unwrap_or_else(BTreeMap::new)
}

pub fn part1<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    best(parse(lines)).len()
}

pub fn part2<'a, I, S>(lines: I) -> Option<i32>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    util::transpose(best(parse(lines)).values())
        .flatten()
        .enumerate()
        .nth(199)
        .map(|(_, (x, y))| 100 * x + y)
}
