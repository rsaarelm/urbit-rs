use std::rc::Rc;
use std::collections::HashMap;
use std::cmp;
use num::{BigUint, FromPrimitive, ToPrimitive, Zero, One};
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
    let end_bit = (start_bit + c) << bloq;

    if start_bit % 8 != 0 || end_bit % 8 != 0 {
        bail!("FIXME cut does not support non-byte offsets yet");
    }

    Ok(d[(start_bit / 8)..(end_bit / 8)].to_vec())
}

#[inline]
fn bit(data: &[u8], pos: usize) -> bool {
    data[pos / 8] & (1 << (pos % 8)) != 0
}

/// Decode a lenght-encoded atom from a bit stream.
pub fn rub(data: Rc<Vec<u8>>, pos: usize) -> JetResult<(usize, BigUint)> {
    // Length of the prefix in bits is the count of initial zeroes before
    // the separator 1.

    let mut p = 0;

    // Assume the first bit is zero even if it isn't.
    let mut k = 1;
    p += 1;

    while !bit(&data, pos + p) {
        k += 1;
        p += 1;
    }
    p += 1;

    // Read the prefix.
    let mut b = 0;
    if k > 1 {
        for i in 0..(k - 2) {
            if bit(&data, pos + p) {
                b += 1 << i;
            }
            p += 1;
        }
        // Add the implicit top 1 to the prefix.
        b += 1 << (k - 2);
    }

    let mut q: BigUint = Default::default();
    for i in 0..b {
        if bit(&data, pos + p) {
            q = q + (BigUint::one() << i);
        }
        p += 1;
    }
    Ok((p, q))
}

/// Decode an encoded cell from a bit stream.
///
/// Return the Nock noun.
pub fn cue(data: Rc<Vec<u8>>) -> JetResult<Noun> {
    let (_, noun) = try!(parse(0, data, &mut Default::default()));
    return Ok(noun);

    fn parse(mut pos: usize,
             data: Rc<Vec<u8>>,
             dict: &mut HashMap<usize, Noun>)
             -> JetResult<(usize, Noun)> {
        let key = pos;
        if bit(&data, pos) {
            pos += 1;
            if !bit(&data, pos) {
                // 10: encode a pair.
                pos += 1;
                let (p, left) = try!(parse(pos, data.clone(), dict));
                pos = p;
                let (p, right) = try!(parse(pos, data.clone(), dict));
                pos = p;

                let ret = Noun::cell(left, right);
                dict.insert(key, ret.clone());
                Ok((pos, ret))
            } else {
                // 11: Repeat element
                // Read the index in bitstream where the value was first
                // encountered.
                let (p, q) = try!(rub(data.clone(), pos));
                pos += p;
                let key = q.to_usize().unwrap();
                if let Some(x) = dict.get(&key) {
                    Ok((pos, x.clone()))
                } else {
                    bail!("Bad cell index")
                }
            }
        } else {
            // Atom.
            let (p, q) = try!(rub(data.clone(), pos));
            pos += p;
            let ret = Noun::from(q);
            dict.insert(key, ret.clone());
            Ok((pos, ret))
        }
    }
}
