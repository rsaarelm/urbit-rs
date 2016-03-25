use num::bigint::BigUint;
use num::One;
use nock::{self, Noun, FromNoun};

// FIXME: Needing to do the get_axis on subject.
// TODO: Error handling that isn't panicing.

pub fn dec(subject: &Noun) -> Noun {
    // XXX: How do we know the axis here?
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let n = BigUint::from_noun(&arg).unwrap();
    Noun::from(n - BigUint::one())
}

pub fn bex(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    // XXX: Will fail if arg is above machine word size.
    let n: usize = FromNoun::from_noun(&arg).unwrap();
    Noun::from(BigUint::one() << n)
}

pub fn add(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (x, y): (BigUint, BigUint) = FromNoun::from_noun(&arg).unwrap();
    Noun::from(x + y)
}

pub fn mul(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (x, y): (BigUint, BigUint) = FromNoun::from_noun(&arg).unwrap();
    Noun::from(x * y)
}

pub fn sub(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (x, y): (BigUint, BigUint) = FromNoun::from_noun(&arg).unwrap();
    Noun::from(x - y)
}

pub fn div(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (x, y): (BigUint, BigUint) = FromNoun::from_noun(&arg).unwrap();
    Noun::from(x / y)
}

pub fn lth(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (x, y): (BigUint, BigUint) = FromNoun::from_noun(&arg).unwrap();
    Noun::from(x < y)
}

pub fn rsh(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (a, b, c): (usize, usize, BigUint) = FromNoun::from_noun(&arg).unwrap();
    Noun::from(c >> ((1 << a) * b))
}

pub fn lsh(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (a, b, c): (usize, usize, BigUint) = FromNoun::from_noun(&arg).unwrap();
    Noun::from(c << ((1 << a) * b))
}

pub fn mod_(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (x, y): (BigUint, BigUint) = FromNoun::from_noun(&arg).unwrap();
    Noun::from(x % y)
}
