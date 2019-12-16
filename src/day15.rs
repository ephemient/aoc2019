use super::intcode::Intcode;
use super::util;
use crossbeam_channel;
use itertools;
use std::collections::{HashSet, VecDeque};
use std::error;
use std::hash::Hash;
use std::ops::{Add, Mul, Sub};
use std::thread;

fn neighbors<T>(pos: &(T, T)) -> Vec<((T, T), i32)>
where
    T: Copy + From<i32> + Add<Output = T> + Sub<Output = T>,
{
    vec![
        ((pos.0, pos.1 - T::from(1)), 1),
        ((pos.0, pos.1 + T::from(1)), 2),
        ((pos.0 - T::from(1), pos.1), 3),
        ((pos.0 + T::from(1), pos.1), 4),
    ]
}

fn path<T>(src: &(T, T), dst: &(T, T), free: &HashSet<(T, T)>) -> Option<Vec<((T, T), i32)>>
where
    T: Copy + Eq + Hash + From<i32> + Add<Output = T> + Sub<Output = T>,
{
    if *src == *dst {
        return Some(vec![]);
    }
    let mut seen: HashSet<(T, T)> = HashSet::new();
    let mut queue: VecDeque<((T, T), Vec<((T, T), i32)>)> = VecDeque::new();
    queue.push_back((*src, vec![]));
    while let Some((pos, mut path)) = queue.pop_back() {
        for step in neighbors(&pos) {
            if step.0 == *dst {
                path.push(step);
                return Some(path);
            }
            if seen.contains(&step.0) || !free.contains(&step.0) {
                continue;
            }
            seen.insert(step.0);
            let mut path2 = path.clone();
            path2.push(step);
            queue.push_back((step.0, path2));
        }
    }
    None
}

fn explore<T>(
    mem: &[T],
) -> Result<Option<((T, T), HashSet<(T, T)>)>, Box<dyn error::Error + Send + Sync>>
where
    T: Copy
        + Default
        + Ord
        + Send
        + Sync
        + Hash
        + From<bool>
        + From<i32>
        + Into<i64>
        + Add<Output = T>
        + Sub<Output = T>
        + Mul<Output = T>
        + 'static,
{
    let (input_send, input_recv) = crossbeam_channel::bounded::<T>(0);
    let (output_send, output_recv) = crossbeam_channel::bounded::<T>(0);
    let mut intcode_mem = mem.to_vec();
    thread::spawn(move || -> Result<_, Box<dyn error::Error + Send + Sync>> {
        Intcode::new(&mut intcode_mem).run(&mut (
            || Ok(input_recv.recv()?),
            |value| Ok(output_send.send(value)?),
        ))
    });

    let mut visited: HashSet<(T, T)> = HashSet::new();
    let mut free: HashSet<(T, T)> = HashSet::new();
    let mut oxygen: Option<(T, T)> = None;
    let mut pending: VecDeque<(T, T)> = VecDeque::new();
    let mut pos = (T::default(), T::default());
    visited.insert(pos);
    pending.push_back(pos);

    while let Some(target) = pending.pop_front() {
        let mut wall = false;
        for (pos2, direction) in path(&pos, &target, &free).ok_or(util::Error)? {
            input_send.send(T::from(direction))?;
            match output_recv.recv()?.into() {
                0 => {
                    wall = true;
                    break;
                }
                2 => oxygen = Some(pos2),
                _ => {}
            }
            pos = pos2;
        }
        if !wall {
            free.insert(pos);
        }
        for (pos2, _) in neighbors(&pos) {
            if !visited.contains(&pos2) {
                visited.insert(pos2);
                pending.push_back(pos2);
            }
        }
    }

    Ok(oxygen.map(|oxygen| (oxygen, free)))
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mem: Vec<i64> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    let (oxygen, free) = explore(&mem)?.ok_or(util::Error)?;
    Ok(path(&(0, 0), &oxygen, &free).ok_or(util::Error)?.len())
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = lines.into_iter().nth(0).ok_or(util::Error)?.as_ref();
    let mem: Vec<i64> = util::parse_many(&line.split(',').collect::<Vec<&str>>())?;
    let (oxygen, free) = explore(&mem)?.ok_or(util::Error)?;
    Ok(itertools::max(
        free.iter()
            .filter_map(|pos| path(&oxygen, pos, &free))
            .map(|path| path.len()),
    )
    .ok_or(util::Error)?)
}
