use super::util;
use std::error;

struct ND {
    started: bool,
    current: Vec<u32>,
    stop: Vec<u32>,
}

impl ND {
    fn new(lo: &str, hi: &str) -> ND {
        let mut current = lo
            .chars()
            .filter_map(|c| c.to_digit(10))
            .collect::<Vec<u32>>();
        let stop = hi
            .chars()
            .filter_map(|c| c.to_digit(10))
            .collect::<Vec<u32>>();
        assert_eq!(current.len(), stop.len());
        for i in 1..current.len() {
            current[i] = current[i - 1].max(current[i]);
        }
        ND {
            started: false,
            current: current,
            stop: stop,
        }
    }
}

impl Iterator for ND {
    type Item = Vec<u32>;

    fn next(&mut self) -> Option<Vec<u32>> {
        if self.started {
            let mut found: Option<usize> = None;
            for (i, c) in self.current.iter().enumerate().rev() {
                if *c < 9u32 {
                    found = Some(i);
                    break;
                }
            }
            let fill = self.current[found?] + 1;
            for c in self.current[found?..].iter_mut() {
                *c = fill;
            }
        } else {
            self.started = true;
        }
        if self.current > self.stop {
            return None;
        }
        return Some(self.current.to_vec());
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let dash = line.chars().position(|c| c == '-').ok_or(util::Error)?;
    return Ok(ND::new(&line[..dash], &line[dash + 1..])
        .filter(|a| {
            for i in 0..a.len() - 1 {
                if a[i] == a[i + 1] {
                    return true;
                }
            }
            return false;
        })
        .count());
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let dash = line.chars().position(|c| c == '-').ok_or(util::Error)?;
    return Ok(ND::new(&line[..dash], &line[dash + 1..])
        .filter(|a| {
            for i in 0..a.len() - 1 {
                if a[i] == a[i + 1]
                    && (i <= 0 || a[i - 1] != a[i])
                    && (i + 2 >= a.len() || a[i + 1] != a[i + 2])
                {
                    return true;
                }
            }
            return false;
        })
        .count());
}
