extern crate num;
extern crate bit_vec;

use std::env;
use std::fmt;
use std::io::prelude::*;
use std::fs::File;
use std::default::Default;
use std::rc::Rc;
use std::collections::HashMap;
use bit_vec::BitVec;
use num::bigint::BigUint;
use num::traits::{One, ToPrimitive};

#[derive(Clone, Debug)]
pub enum Noun {
    Cell(Rc<Noun>, Rc<Noun>),
    Atom(BigUint),
}

use Noun::*;

impl fmt::Display for Noun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Atom(ref n) => return dot_separators(f, &n),
            &Cell(ref a, ref b) => {
                try!(write!(f, "[{} ", a));
                // List pretty-printer.
                let mut cur = b;
                loop {
                    match **cur {
                        Cell(ref a, ref b) => {
                            try!(write!(f, "{} ", a));
                            cur = &b;
                        }
                        Atom(ref n) => {
                            try!(dot_separators(f, &n));
                            return write!(f, "]");
                        }
                    }
                }
            }
        }

        fn dot_separators<T: fmt::Display>(f: &mut fmt::Formatter, item: &T) -> fmt::Result {
            let s = format!("{}", item);
            let phase = s.len() % 3;
            for (i, c) in s.chars().enumerate() {
                if i > 0 && i % 3 == phase {
                    try!(write!(f, "."));
                }
                try!(write!(f, "{}", c));
            }
            Ok(())
        }
    }
}

fn rub(bits: &BitVec, pos: usize) -> (usize, BigUint) {
    // Length of the prefix in bits is the count of initial zeroes before
    // the separator 1.

    let mut p = 0;

    // Assume the first bit is zero even if it isn't.
    let mut k = 1;
    p += 1;

    while !bits[pos + p] {
        k += 1;
        p += 1;
    }
    p += 1;

    // Read the prefix.
    let mut b = 0;
    if k > 1 {
        for i in 0..(k - 2) {
            if bits[pos + p] {
                b += 1 << i;
            }
            p += 1;
        }
        // Add the implicit top 1 to the prefix.
        b += 1 << (k - 2);
    }

    let mut q: BigUint = Default::default();
    for i in 0..b {
        if bits[pos + p] {
            q = q + (BigUint::one() << i);
        }
        p += 1;
    }
    (p, q)
}

pub fn cue(bits: &BitVec) -> (Rc<Noun>, u64) {
    let (_, noun, size) = parse(0, bits, &mut HashMap::new());
    return (noun, size);

    fn parse(mut pos: usize, bits: &BitVec, dict: &mut HashMap<usize, (Rc<Noun>, u64)>) -> (usize, Rc<Noun>, u64) {
        let key = pos;
        if bits[pos] {
            pos += 1;
            if !bits[pos] {
                // 10: encode a pair.
                pos += 1;
                let (p, left, s1) = parse(pos, bits, dict);
                pos = p;
                let (p, right, s2) = parse(pos, bits, dict);
                pos = p;

                let ret = Rc::new(Noun::Cell(left, right));
                let size = s1 + s2;
                dict.insert(key, (ret.clone(), size));
                (pos, ret, size)
            } else {
                // 11: Repeat element
                // Read the index in bitstream where the value was first
                // encountered.
                let (p, q) = rub(&bits, pos);
                pos += p;
                let key = q.to_usize().unwrap();
                let (noun, size) = dict.get(&key).unwrap().clone();
                (pos, noun, size)
            }
        } else {
            // Atom.
            let (p, q) = rub(&bits, pos);
            pos += p;
            let ret = Rc::new(Noun::Atom(q));
            dict.insert(key, (ret.clone(), 1));
            (pos, ret, 1)
        }
    }
}

fn main() {
    let mut file = File::open(env::args().nth(1).expect("Usage scan [urbit.pill]")).unwrap();

    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();

    // Guarantee that the buffer is made of 32-bit chunks.
    while buf.len() % 4 != 0 {
        buf.push(0);
    }

    // Reverse bits of each byte to be little-endian all the way.
    for i in 0..buf.len() {
        let b = &mut buf[i];
        *b = (*b & 0xF0) >> 4 | (*b & 0x0F) << 4;
        *b = (*b & 0xCC) >> 2 | (*b & 0x33) << 2;
        *b = (*b & 0xAA) >> 1 | (*b & 0x55) << 1;
    }

    let bits = BitVec::from_bytes(&buf);

    let (_cell, size) = cue(&bits);

    println!("Pillfile {} consists of {} atoms", env::args().nth(1).unwrap(), size);
}
