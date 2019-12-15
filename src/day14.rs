use super::util;
use regex::Regex;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::error;
use std::ops::{Add, Div, Mul, Sub};
use std::str::FromStr;

type Rules<T> = HashMap<String, (T, HashMap<String, T>)>;

fn produce<T>(rules: &Rules<T>, quantity: T) -> T
where
    T: Clone
        + Default
        + Ord
        + From<i32>
        + Add<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + Div<Output = T>,
{
    let ore = "ORE".to_string();
    let mut mats = HashMap::new();
    mats.insert("FUEL".to_string(), quantity);
    while let Some((key, n)) = mats
        .iter()
        .filter_map(|(key, n)| {
            if *key != ore && *n > T::default() {
                Some((key.clone(), n.clone()))
            } else {
                None
            }
        })
        .nth(0)
    {
        let (m, srcs) = &rules[&key];
        let x = (n.clone() + m.clone() - T::from(1)) / m.clone();
        mats.insert(key, n - m.clone() * x.clone());
        for (k, v) in srcs.iter() {
            let y = mats.entry(k.to_string()).or_default();
            *y = y.clone() + v.clone() * x.clone();
        }
    }
    mats.get(&ore).cloned().unwrap_or_default()
}

fn parse<'a, I, S, T>(lines: I) -> Result<Rules<T>, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
    T: Clone + FromStr,
    <T as FromStr>::Err: error::Error + Send + Sync + 'static,
{
    lazy_static! {
        static ref RE: Regex = Regex::new(r#"(\d+) (\w+)"#).unwrap();
    }
    lines
        .into_iter()
        .map(|line| {
            let mut items = RE
                .captures_iter(line.as_ref())
                .map(|cap| -> Result<_, <T as FromStr>::Err> {
                    Ok((cap[2].to_string(), cap[1].parse::<T>()?))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let (key, n) = items.pop().ok_or(util::Error)?;
            Ok((key, (n, items.iter().cloned().collect())))
        })
        .collect()
}

pub fn part1<'a, I, S>(lines: I) -> Result<i64, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(produce(&parse::<_, _, i64>(lines)?, 1))
}

pub fn part2<'a, I, S>(lines: I) -> Result<i64, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let rules = parse::<_, _, i64>(lines)?;
    let target = 1_000_000_000_000i64;
    let mut good = target / produce(&rules, 1);
    let mut bad: Option<i64> = None;
    while bad.map_or(true, |bad| good < bad - 1) {
        let mid = bad.map_or_else(|| good * 2, |bad| (good + bad) / 2);
        match produce(&rules, mid).cmp(&target) {
            Ordering::Less => good = mid,
            Ordering::Equal => return Ok(mid),
            Ordering::Greater => bad = Some(mid),
        }
    }
    Ok(good)
}
