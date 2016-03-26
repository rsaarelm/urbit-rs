#![feature(alloc_system)]

extern crate alloc_system;
extern crate bit_vec;
extern crate rand;
extern crate num;
extern crate fnv;
#[macro_use]
extern crate nock;

use rand::Rng;
use std::io::{self, Write};
use std::collections::HashMap;
use bit_vec::BitVec;
use num::bigint::BigUint;
use num::traits::{One, ToPrimitive};
use nock::{Noun, NockResult, NockError, Shape, FromNoun};
use nock::{DigitSlice, FromDigits};

mod jet;

const SPOT_TEST_JETS: bool = false;

/// An Urbit virtual machine.
pub struct VM {
    jets: HashMap<Noun, fn(&Noun) -> Noun>,
    // Just so we don't make noise about an unimplemented jet more than once.
    unimplemented_jets: HashMap<String, usize>,
    ticks: u64,
}

impl VM {
    pub fn new() -> VM {
        VM {
            jets: HashMap::new(),
            unimplemented_jets: HashMap::new(),
            ticks: 0,
        }
    }

    pub fn nock_on(&mut self, mut subject: Noun, mut formula: Noun) -> NockResult {
        self.tick();

        // XXX: Copy-pasted a bunch from the reference implementation at
        // nock-rs. The nock_on machinery needs to be reasonably monolithical
        // because we're doing the looping manual tail call elimination.
        loop {
            if let Shape::Cell(ops, tail) = formula.clone().get() {
                match ops.as_u32() {
                    // Axis
                    Some(0) => return nock::get_axis(&tail, &subject),

                    // Just
                    Some(1) => return Ok(tail.clone()),

                    // Fire
                    Some(2) => {
                        match tail.get() {
                            Shape::Cell(ref b, ref c) => {
                                let p = try!(self.nock_on(subject.clone(), (*b).clone()));
                                let q = try!(self.nock_on(subject, (*c).clone()));
                                subject = p;
                                formula = q;
                                continue;
                            }
                            _ => return Err(NockError),
                        }
                    }

                    // Depth
                    Some(3) => {
                        let p = try!(self.nock_on(subject.clone(), (*tail).clone()));
                        return match p.get() {
                            Shape::Cell(_, _) => Ok(Noun::from(0u32)),
                            _ => Ok(Noun::from(1u32)),
                        };
                    }

                    // Bump
                    Some(4) => {
                        let p = try!(self.nock_on(subject.clone(), (*tail).clone()));
                        return match p.get() {
                            Shape::Atom(ref x) => {
                                // TODO: Non-bignum optimization
                                Ok(Noun::from(BigUint::from_digits(x).unwrap() + BigUint::one()))
                            }
                            _ => Err(NockError),
                        };
                    }

                    // Same
                    Some(5) => {
                        let p = try!(self.nock_on(subject.clone(), (*tail).clone()));
                        return match p.get() {
                            Shape::Cell(ref a, ref b) => {
                                if a == b {
                                    // Yes.
                                    return Ok(Noun::from(0u32));
                                } else {
                                    // No.
                                    return Ok(Noun::from(1u32));
                                }
                            }
                            _ => return Err(NockError),
                        };
                    }

                    // If
                    Some(6) => {
                        if let Some((b, c, d)) = tail.get_122() {
                            let p = try!(self.nock_on(subject.clone(), (*b).clone()));
                            match p.get() {
                                Shape::Atom(ref x) => {
                                    if x == &0u32.as_digits() {
                                        formula = (*c).clone();
                                    } else if x == &1u32.as_digits() {
                                        formula = (*d).clone();
                                    } else {
                                        return Err(NockError);
                                    }
                                }
                                _ => return Err(NockError),
                            }
                            continue;
                        } else {
                            return Err(NockError);
                        }
                    }

                    // Compose
                    Some(7) => {
                        match tail.get() {
                            Shape::Cell(ref b, ref c) => {
                                let p = try!(self.nock_on(subject.clone(), (*b).clone()));
                                subject = p;
                                formula = (*c).clone();
                                continue;
                            }
                            _ => return Err(NockError),
                        }
                    }

                    // Push
                    Some(8) => {
                        match tail.get() {
                            Shape::Cell(ref b, ref c) => {
                                let p = try!(self.nock_on(subject.clone(), (*b).clone()));
                                subject = Noun::cell(p, subject);
                                formula = (*c).clone();
                                continue;
                            }
                            _ => return Err(NockError),
                        }
                    }

                    // Call
                    Some(9) => {
                        match tail.get() {
                            Shape::Cell(ref axis, ref c) => {
                                // Construct core.
                                subject = try!(self.nock_on(subject.clone(), (*c).clone()));
                                // Fetch from core using axis.
                                formula = try!(nock::get_axis(axis, &subject));

                                let x = self.jets.get(&formula).map(|&x| x);
                                if let Some(f) = x {
                                    let result = f(&subject);
                                    // Do random spot-checks against the actual nock code.
                                    if SPOT_TEST_JETS &&
                                       rand::thread_rng().gen_range(0.0, 1.0) < 0.00001 {
                                        let verify = self.nock_on(subject.clone(), formula.clone())
                                                         .unwrap();
                                        assert!(verify == result,
                                                "{} jet returned {:?}, expected {:?}",
                                                symhash(&formula),
                                                result,
                                                verify);
                                        print!("+");
                                    }
                                    return Ok(f(&subject));
                                }

                                continue;
                            }
                            _ => return Err(NockError),
                        }
                    }

                    // Hint
                    Some(10) => {
                        match tail.get() {
                            Shape::Cell(ref hint, ref c) => {
                                let (id, clue) = match hint.get() {
                                    Shape::Cell(ref p, ref q) => {
                                        (p.clone(),
                                         try!(self.nock_on(subject.clone(), (*q).clone())))
                                    }
                                    Shape::Atom(_) => (hint.clone(), Noun::from(0u32)),
                                };

                                // TODO: Handle other hint types than %fast.
                                if String::from_noun(id).unwrap() == "fast" {
                                    let core = try!(self.nock_on(subject.clone(), (*c).clone()));
                                    if let Ok((name, axis, hooks)) = parse_fast_clue(&clue) {
                                        if let Shape::Cell(ref battery, _) = core.get() {
                                            try!(self.register(battery, name, axis, hooks));
                                        } else {
                                            return Err(NockError);
                                        }
                                    } else {
                                        // println!("Unparseable clue...");
                                    }

                                    return Ok(core);
                                }

                                formula = (*c).clone();
                                continue;
                            }
                            _ => return Err(NockError),
                        }
                    }

                    // Unhandled opcode
                    Some(_) => {
                        return Err(NockError);
                    }

                    None => {
                        if let Shape::Cell(_, _) = ops.get() {
                            // Autocons
                            let a = try!(self.nock_on(subject.clone(), ops.clone()));
                            let b = try!(self.nock_on(subject, tail.clone()));
                            return Ok(Noun::cell(a, b));
                        } else {
                            return Err(NockError);
                        }
                    }
                }
            } else {
                return Err(NockError);
            }
        }
    }

    pub fn register(&mut self,
                    battery: &Noun,
                    name: String,
                    axis: u32,
                    hooks: Vec<(String, Noun)>)
                    -> Result<(), NockError> {
        if let Some(f) = match &name[..] {
            "dec" => Some(jet::dec as fn(&Noun) -> Noun),
            "bex" => Some(jet::bex as fn(&Noun) -> Noun),
            "add" => Some(jet::add as fn(&Noun) -> Noun),
            "mul" => Some(jet::mul as fn(&Noun) -> Noun),
            "sub" => Some(jet::sub as fn(&Noun) -> Noun),
            "div" => Some(jet::div as fn(&Noun) -> Noun),
            "lth" => Some(jet::lth as fn(&Noun) -> Noun),
            "rsh" => Some(jet::rsh as fn(&Noun) -> Noun),
            "lsh" => Some(jet::lsh as fn(&Noun) -> Noun),
            "mod" => Some(jet::mod_ as fn(&Noun) -> Noun),
            _ => None,
        } {
            let key = (*battery).clone();
            if !self.jets.contains_key(&key) {
                println!("{} jetting '{}' ({})", symhash(&key), name, axis);
                self.jets.insert(key, f);
            }
        } else {
            if !self.unimplemented_jets.contains_key(&name) {
                println!("Unknown jet function {}: {:?}", name, hooks);
                self.unimplemented_jets.insert(name, 0);
            } else {
                //if let Some(x) = self.unimplemented_jets.get_mut(&name) {
                //    *x += 1;
                //    if *x % 10000 == 0 {
                //        // Complain about common unjetted words.
                //        print!(" {} ", name);
                //    }
                //} else {
                //    panic!("Invalid hash state");
                //}
            }
        }
        Ok(())
    }

    fn tick(&mut self) {
        self.ticks += 1;
        if self.ticks % 1000000 == 0 {
            self.ticks = 0;
            print!(".");
            let _ = io::stdout().flush();
        }
    }
}

fn parse_fast_clue(clue: &Noun) -> Result<(String, u32, Vec<(String, Noun)>), NockError> {
    if let Some((ref name, ref axis_formula, ref hooks)) = clue.get_122() {
        let chum = try!(String::from_noun(name).map_err(|_| NockError));

        let axis = if let Shape::Cell(ref a, ref b) = axis_formula.get() {
            if let (Some(1), Some(0)) = (a.as_u32(), b.as_u32()) {
                0
            } else if let (Some(0), Some(axis)) = (a.as_u32(), b.as_u32()) {
                axis
            } else {
                return Err(NockError);
            }
        } else {
            return Err(NockError);
        };

        let hooks: Vec<(String, Noun)> = try!(FromNoun::from_noun(hooks).map_err(|_| NockError));

        Ok((chum, axis, hooks))
    } else {
        Err(NockError)
    }
}

/// Compute a hash value for a noun using the fast fnv hasher.
pub fn hash(noun: &Noun) -> u64 {
    use std::hash::{Hasher, Hash};

    let mut fnv = fnv::FnvHasher::default();
    noun.hash(&mut fnv);
    fnv.finish()
}

/// A human-readable hash version.
pub fn symhash(noun: &Noun) -> String {
    fn proquint(buf: &mut String, mut b: u16) {
        const C: [char; 16] = ['b', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'r', 's',
                               't', 'v', 'z'];
        const V: [char; 4] = ['a', 'i', 'o', 'u'];
        buf.push(C[(b % 16) as usize]);
        b >>= 4;
        buf.push(V[(b % 4) as usize]);
        b >>= 2;
        buf.push(C[(b % 16) as usize]);
        b >>= 4;
        buf.push(V[(b % 4) as usize]);
        b >>= 2;
        buf.push(C[(b % 16) as usize]);
    }

    let mut hash = hash(noun);
    let mut ret = String::new();
    for _ in 0..3 {
        proquint(&mut ret, (hash % 0xFFFF) as u16);
        hash >>= 16;
        ret.push('-')
    }
    proquint(&mut ret, (hash % 0xFFFF) as u16);

    ret
}

/// Unpack the data of an Urbit pillfile into a Nock noun.
pub fn unpack_pill(mut buf: Vec<u8>) -> Result<Noun, &'static str> {
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

    cue(&bits)
}

/// Decode a lenght-encoded atom from a bit stream.
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

/// Decode an encoded cell from a bit stream.
///
/// Return the Nock noun.
fn cue(bits: &BitVec) -> Result<Noun, &'static str> {
    let (_, noun) = try!(parse(0, bits, &mut Default::default()));
    return Ok(noun);

    fn parse(mut pos: usize,
             bits: &BitVec,
             dict: &mut HashMap<usize, Noun>)
             -> Result<(usize, Noun), &'static str> {
        let key = pos;
        if bits[pos] {
            pos += 1;
            if !bits[pos] {
                // 10: encode a pair.
                pos += 1;
                let (p, left) = try!(parse(pos, bits, dict));
                pos = p;
                let (p, right) = try!(parse(pos, bits, dict));
                pos = p;

                let ret = Noun::cell(left, right);
                dict.insert(key, ret.clone());
                Ok((pos, ret))
            } else {
                // 11: Repeat element
                // Read the index in bitstream where the value was first
                // encountered.
                let (p, q) = rub(&bits, pos);
                pos += p;
                let key = q.to_usize().unwrap();
                if let Some(x) = dict.get(&key) {
                    Ok((pos, x.clone()))
                } else {
                    Err("Bad cell index")
                }
            }
        } else {
            // Atom.
            let (p, q) = rub(&bits, pos);
            pos += p;
            let ret = Noun::from(q);
            dict.insert(key, ret.clone());
            Ok((pos, ret))
        }
    }
}

#[cfg(test)]
mod test {
    use nock::Noun;
    use super::unpack_pill;

    #[test]
    fn test_read_pill() {
        let noun: Noun = "[18.446.744.073.709.551.616 8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 \
                          2] [4 0 6] 0 7] 9 2 0 1]"
                             .parse()
                             .expect("Nock parse error");
        let pill: &[u8] = &[0x01, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06,
                            0xc1, 0x62, 0x83, 0x60, 0x71, 0xd8, 0x85, 0x5b, 0xe2, 0x87, 0x99,
                            0xd8, 0x0d, 0xc1, 0x1a, 0x24, 0x43, 0x96, 0xc8, 0x86, 0x10, 0x1d,
                            0xc2, 0x32, 0x48, 0x86, 0x4c, 0x06];
        let unpacked = unpack_pill(pill.to_vec()).expect("Pill unpack failed");
        assert_eq!(noun, unpacked);
    }

    #[test]
    fn test_cue_stack() {
        let noun: Noun = "[1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 \
                          0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]"
                             .parse()
                             .expect("Nock parse error");
        let pill: &[u8] = "qffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\
                           fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff\
                           fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffj"
                              .as_bytes();
        let unpacked = unpack_pill(pill.to_vec()).expect("Pill unpack failed");
        assert_eq!(noun, unpacked);
    }
}
