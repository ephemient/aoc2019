use super::intcode::Intcode;
use super::util;
use crossbeam_channel;
use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::error;
use std::mem;
use std::thread;

fn walk(
    mem: &mut Vec<i64>,
    start: bool,
) -> Result<HashMap<(i32, i32), bool>, Box<dyn error::Error + Send + Sync>> {
    let (input_sender, input_receiver) = crossbeam_channel::unbounded::<i64>();
    let (output_sender, output_receiver) = crossbeam_channel::bounded::<i64>(0);
    let (stop_sender, stop_receiver) = crossbeam_channel::bounded::<()>(0);
    let result = thread::spawn(
        move || -> Result<HashMap<(i32, i32), bool>, Box<dyn error::Error + Send + Sync>> {
            let mut result: HashMap<(i32, i32), bool> = HashMap::new();
            result.insert((0, 0), start);
            let mut x = 0;
            let mut y = 0;
            let mut dx = 0;
            let mut dy = 1;
            loop {
                let is_true = result.get(&(x, y)).map_or(false, |b| *b);
                select! {
                    send(input_sender, is_true as i64) -> res => res?,
                    recv(stop_receiver) -> _ => break,
                }
                select! {
                    recv(output_receiver) -> color => {
                        result.insert((x, y), color? != 0);
                    }
                    recv(stop_receiver) -> _ => break,
                }
                select! {
                    recv(output_receiver) -> turn => {
                        mem::swap(&mut dx, &mut dy);
                        if turn? == 0 {
                            dx = -dx;
                        } else {
                            dy = -dy;
                        }
                        x += dx;
                        y += dy;
                    }
                    recv(stop_receiver) -> _ => break,
                }
            }
            return Ok(result);
        },
    );
    Intcode::new(mem).run::<_, _, Box<dyn error::Error + Send + Sync>>(
        || Ok(input_receiver.recv()?),
        |value| Ok(output_sender.send(value)?),
    )?;
    stop_sender.send(())?;
    return result.join().map_err(|_| util::Error)?;
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i64> =
        util::parse_many(&line.split(',').map(|s| s.clone()).collect::<Vec<&str>>())?;
    return Ok(walk(&mut mem, false)?.len());
}

pub fn part2<'a, I, S>(lines: I) -> Result<String, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mut mem: Vec<i64> =
        util::parse_many(&line.split(',').map(|s| s.clone()).collect::<Vec<&str>>())?;
    let result = walk(&mut mem, true)?
        .into_iter()
        .filter(|(_, v)| *v)
        .map(|(k, _)| k)
        .collect::<HashSet<_>>();
    let (xs, ys) = result.clone().into_iter().unzip::<_, _, Vec<_>, Vec<_>>();
    let (min_x, max_x) = xs.into_iter().minmax().into_option().ok_or(util::Error)?;
    let (min_y, max_y) = ys.into_iter().minmax().into_option().ok_or(util::Error)?;
    return Ok((min_y..=max_y)
        .rev()
        .map(|y| {
            (min_x..=max_x)
                .map(|x| match result.contains(&(x, y)) {
                    true => '\u{2593}',
                    false => '\u{2591}',
                })
                .collect::<String>()
        })
        .join("\n"));
}
