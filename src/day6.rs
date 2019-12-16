use collecting_hashmap::CollectingHashMap;
use std::cmp;
use std::collections::HashMap;
use std::error;
use std::hash::Hash;
use std::iter;

struct Checksums<'a, T>
where
    T: Hash + Eq,
{
    orbits: &'a CollectingHashMap<T, T>,
    cache: HashMap<T, i32>,
}

impl<'a, T> Checksums<'a, T>
where
    T: Hash + Eq + Clone,
{
    fn new(orbits: &'a CollectingHashMap<T, T>) -> Checksums<'a, T> {
        Checksums {
            orbits,
            cache: HashMap::new(),
        }
    }

    fn checksum(&mut self, key: &T) -> i32 {
        match self.cache.get(key) {
            Some(cached) => *cached,
            _ => {
                let value = match self.orbits.get_all(key) {
                    Some(children) => children.iter().map(|x: &T| self.checksum(x) + 1).sum(),
                    _ => 0,
                };
                self.cache.insert(key.clone(), value);
                value
            }
        }
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let orbits = lines
        .into_iter()
        .filter_map(|s| {
            let line = s.as_ref();
            let sep = line.find(')')?;
            Some((line[..sep].to_string(), line[sep + 1..].to_string()))
        })
        .collect::<CollectingHashMap<_, _>>();
    let mut checksums = Checksums::new(&orbits);
    Ok(orbits.keys().map(|x| checksums.checksum(x)).sum())
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn error::Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let rorbits = lines
        .into_iter()
        .filter_map(|s| {
            let line = s.as_ref();
            let sep = line.find(')')?;
            Some((line[sep + 1..].to_string(), line[..sep].to_string()))
        })
        .collect::<HashMap<_, _>>();
    let succ = |x: &String| rorbits.get(x).cloned();
    let san = iter::successors(Some("SAN".to_string()), succ).collect::<Vec<_>>();
    let you = iter::successors(Some("YOU".to_string()), succ).collect::<Vec<_>>();
    let common = (0..cmp::min(san.len(), you.len()))
        .find(|i| san[san.len() - i - 1] != you[you.len() - i - 1])
        .unwrap_or_else(|| cmp::min(san.len(), you.len()));
    Ok(san.len() + you.len() - 2 * (common + 1))
}
