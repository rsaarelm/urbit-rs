use std::rc::Rc;
use std::cmp;
use num::{BigUint, FromPrimitive, Zero, One};
use jet::{JetError, JetResult};
use nock::{self, Noun};

/// Return with an error from a jet.
macro_rules! bail( ($($arg:tt)*) => ( return Err(JetError(format!($($arg)*))); ));

pub fn dec(n: BigUint) -> JetResult<BigUint> {
    if n == BigUint::zero() {
        bail!("dec 0");
    }
    Ok(n - BigUint::one())
}

/// Binary exponent, compute 2^a.
pub fn bex(a: usize) -> JetResult<BigUint> {
    Ok(BigUint::one() << a)
}

pub fn add(a: BigUint, b: BigUint) -> JetResult<BigUint> {
    Ok(a + b)
}

pub fn mul(a: BigUint, b: BigUint) -> JetResult<BigUint> {
    Ok(a * b)
}

pub fn sub(a: BigUint, b: BigUint) -> JetResult<BigUint> {
    if b > a {
        bail!("sub goes negative");
    }
    Ok(a - b)
}

pub fn div(a: BigUint, b: BigUint) -> JetResult<BigUint> {
    if b == BigUint::zero() {
        bail!("div by zero");
    }
    Ok(a / b)
}

/// Less-than.
pub fn lth(a: BigUint, b: BigUint) -> JetResult<bool> {
    Ok(a < b)
}

pub fn rsh(bloq: usize, b: usize, c: BigUint) -> JetResult<BigUint> {
    Ok(c >> (b << bloq))
}

pub fn lsh(bloq: usize, b: usize, c: BigUint) -> JetResult<BigUint> {
    Ok(c << (b << bloq))
}

pub fn mod_(a: BigUint, b: BigUint) -> JetResult<BigUint> {
    Ok(a % b)
}

pub fn mix(a: BigUint, b: BigUint) -> JetResult<BigUint> {
    Ok(a ^ b)
}

/// Counts bloq length of atom.
pub fn met(bloq: usize, a: Rc<Vec<u8>>) -> JetResult<BigUint> {
    let bits = nock::msb(&*a) + (1 << bloq) - 1;
    Ok(BigUint::from_usize(bits / (1 << bloq)).unwrap())
}

pub fn mug(a: Noun) -> JetResult<u32> {
    Ok(a.mug())
}

/// Produces an atom by taking the last `b` blocks of size `bloq` from `c`.
pub fn end(bloq: usize, b: usize, c: Rc<Vec<u8>>) -> JetResult<Vec<u8>> {
    // Using Rc<Vec<u8>> lets you access large atoms without copying the atom
    // contents.

    assert!(bloq == 3);
    let bits = (1 << bloq) * b;
    let bytes = cmp::min(((bits + 7) / 8), c.len());

    let mut ret = c[0..bytes].to_vec();
    // Snip off last bits if bit count isn't byte multiple.
    let trailing = bits % 8;
    if trailing != 0 {
        assert!(ret.len() > 0);
        let last_idx = ret.len() - 1;
        ret[last_idx] &= (1 << trailing) - 1;
    }
    Ok(ret)
}

/// Slices `c` blocks of size `bloq` that are `b` blocks from the end of `d`.
pub fn cut(bloq: usize, (b, c): (usize, usize), d: Rc<Vec<u8>>) -> JetResult<Vec<u8>> {
    let start_bit = b << bloq;
    let end_bit = start_bit + c << bloq;

    if start_bit % 8 != 0 || end_bit % 8 != 0 {
        bail!("FIXME cut does not support non-byte offsets yet");
    }

    Ok(d[(start_bit / 8)..(end_bit / 8)].to_vec())
}
