use super::intcode::Intcode;
use super::util;
use crossbeam_channel;
use itertools::{self, Itertools};
use std::error;
use std::thread;

fn amplify(mem: &[i32], order: &[i32]) -> Result<Option<i32>, Box<dyn error::Error + Send + Sync>> {
    let channels = order
        .iter()
        .enumerate()
        .map(|(i, n)| -> Result<_, crossbeam_channel::SendError<i32>> {
            let (s, r) = crossbeam_channel::unbounded::<i32>();
            s.send(*n)?;
            if i == 0 {
                s.send(0)?;
            }
            Ok((s, r))
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok(*(0..order.len())
        .map(|i| {
            let mut mem = mem.to_vec();
            let s = channels[(i + 1) % order.len()].0.clone();
            let r = channels[i].1.clone();
            thread::spawn(move || -> Result<_, Box<dyn error::Error + Send + Sync>> {
                let mut output: Option<i32> = None;
                Intcode::new(&mut mem).run(&mut (
                    || Ok(r.recv()?),
                    |value| -> Result<(), Box<dyn error::Error + Send + Sync>> {
                        output = Some(value);
                        Ok(s.send(value)?)
                    },
                ))?;
                Ok(output)
            })
        })
        .collect::<Vec<_>>()
        .into_iter()
        .map(|handle| -> Result<_, Box<dyn error::Error + Send + Sync>> {
            Ok(handle.join().map_err(|_| util::Error)??)
        })
        .collect::<Result<Vec<_>, _>>()?
        .last()
        .ok_or(util::Error)?)
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mem: Vec<i32> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    Ok(itertools::max(
        (0..5)
            .permutations(5)
            .filter_map(|order| amplify(&mem, &order).transpose())
            .collect::<Result<Vec<i32>, _>>()?,
    )
    .ok_or(util::Error)?)
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mem: Vec<i32> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    Ok(itertools::max(
        (5..10)
            .permutations(5)
            .filter_map(|order| amplify(&mem, &order).transpose())
            .collect::<Result<Vec<i32>, _>>()?,
    )
    .ok_or(util::Error)?)
}
