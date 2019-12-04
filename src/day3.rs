use std::collections::HashMap;

fn to_points(input: &str) -> impl Iterator<Item = (i32, i32)> + '_ {
    input
        .split(',')
        .scan((0, 0), |p, s| {
            let step = match s.chars().next()? {
                'U' => (0, 1),
                'L' => (-1, 0),
                'D' => (0, -1),
                'R' => (1, 0),
                _ => return None,
            };
            let n = s[1..].parse::<u32>().ok()?;
            return Some(
                (0u32..n)
                    .scan(p, |p, _| {
                        p.0 += step.0;
                        p.1 += step.1;
                        return Some(p.clone());
                    })
                    .collect::<Vec<(i32, i32)>>(),
            );
        })
        .flatten()
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut result: Option<usize> = None;
    let mut field: HashMap<(i32, i32), usize> = HashMap::new();
    for (i, line) in lines.into_iter().enumerate() {
        for p in to_points(line.as_ref()) {
            match field.insert(p, i) {
                Some(j) if i != j => {
                    let d = p.0.abs() as usize + p.1.abs() as usize;
                    if result.filter(|o| *o <= d).is_none() {
                        result = Some(d)
                    }
                }
                _ => {}
            }
        }
    }
    return result;
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut result: Option<usize> = None;
    let mut field: HashMap<(i32, i32), usize> = HashMap::new();
    let mut distances: HashMap<(i32, i32), usize> = HashMap::new();
    for (i, line) in lines.into_iter().enumerate() {
        for (d, p) in to_points(line.as_ref()).enumerate() {
            if field.insert(p, i) == Some(i) {
                continue;
            }
            let distance = d + 1;
            distances
                .entry(p)
                .and_modify(|other| {
                    if result.filter(|o| *o <= *other + distance).is_none() {
                        result = Some(*other + distance)
                    }
                    *other = distance.min(*other);
                })
                .or_insert(distance);
        }
    }
    return result;
}
