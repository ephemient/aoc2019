use std::error;
use std::io;
use std::str::FromStr;

pub fn to_ioerror<E>(error: E) -> io::Error
where
    E: Into<Box<dyn error::Error+Send+Sync>>
{
    return io::Error::new(io::ErrorKind::Other, error);
}

pub fn parse_many<'a, F, I, S>(lines: I) -> Result<Vec<F>, <F as FromStr>::Err>
where
    F: FromStr,
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    return Result::Ok(lines
        .into_iter()
        .map(|s| s.as_ref().parse::<F>())
        .collect::<Result<Vec<F>, <F as FromStr>::Err>>()?
    )
}
