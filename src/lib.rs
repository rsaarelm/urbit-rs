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
use num::{BigUint, One, ToPrimitive};
use nock::{Noun, NockResult, NockError, Shape, FromNoun};
use nock::{DigitSlice, FromDigits};
use jet::Jet;

mod jet;

const SPOT_TEST_JETS: bool = false;

/// An Urbit virtual machine.
pub struct VM {
    jets: HashMap<Noun, jet::Jet>,
    ticks: u64,
    test_mode: bool,
}

impl VM {
    pub fn new() -> VM {
        VM {
            jets: HashMap::new(),
            ticks: 0,
            test_mode: false,
        }
    }

    pub fn nock_on(&mut self, mut subject: Noun, mut formula: Noun) -> NockResult {
        if !self.test_mode {
            self.tick();
        }

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

                                let mut jetted_result = None;

                                if let Some(jet) = self.jets.get_mut(&formula) {
                                    jet.calls += 1;

                                    if let Some(f) = jet.jet {
                                        jetted_result = Some(f(&subject));
                                    }
                                }

                                if let Some(result) = jetted_result {
                                    // Randomly check jets against naive nock.
                                    let mut do_check = SPOT_TEST_JETS &&
                                                   rand::thread_rng().gen_range(0.0, 1.0) < 0.00001;
                                    //if self.jets.get(&formula).map_or("", |x| &x.name[..]) == "cut" { do_check = true; }
                                    if self.test_mode { do_check = false; }

                                    if do_check {
                                        self.test_mode = true;
                                        let verify = self.nock_on(subject.clone(), formula.clone())
                                                         .unwrap();
                                        assert!(verify == result,
                                                "{} jet returned {:?}, expected {:?}",
                                                symhash(&formula),
                                                result,
                                                verify);
                                        print!("+");
                                        self.test_mode = false;
                                    }

                                    return Ok(result);
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
                                            if !self.jets.contains_key(battery) {
                                                let jet = Jet::new(name,
                                                                   (*battery).clone(),
                                                                   axis,
                                                                   hooks);
                                                self.jets.insert((*battery).clone(), jet);
                                            }
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

    fn tick(&mut self) {
        self.ticks = self.ticks.wrapping_add(1);
        if self.ticks % 1000000 == 0 {
            print!(".");
            let _ = io::stdout().flush();
        }

        if self.ticks % 100000000 == 0 {
            println!("");
            self.print_status();
        }
    }

    pub fn print_status(&self) {
        let mut total_count = 0;
        let mut jets: Vec<&Jet> = self.jets.iter().map(|(_, x)| x).collect();
        jets.sort_by(|a, b| b.calls.cmp(&a.calls));
        for jet in jets.iter() {
            if jet.calls < 100 || (jet.calls as f32) / (total_count as f32) < 1e-6 {
                // Don't care about the little things
                println!(" ...");
                break;
            }
            println!("{}{} called {} times",
                     if jet.jet.is_some() { '*' } else { ' ' },
                     jet.name, jet.calls);
            total_count += jet.calls;
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

    let mut hash = noun.mug();
    let mut ret = String::new();
    proquint(&mut ret, (hash % 0xFFFF) as u16);
    hash >>= 16;
    ret.push('-');
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

    #[test]
    fn test_read_pill() {
        use super::unpack_pill;

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
        use super::unpack_pill;

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


    fn produces(input: &str, output: &str) {
        use super::VM;
        use nock::Shape;

        let (s, f) = match input.parse::<Noun>() {
            Err(_) => panic!("Parsing failed"),
            Ok(x) => {
                if let Shape::Cell(ref s, ref f) = x.get() {
                    ((*s).clone(), (*f).clone())
                } else {
                    panic!("Unnockable input")
                }
            }
        };
        let mut vm = VM::new();
        assert_eq!(format!("{}", vm.nock_on(s, f).ok().expect("Eval failed")),
                   output);
    }


    #[test]
    fn test_autocons() {
        produces("[42 [4 0 1] [3 0 1]]", "[43 1]");
    }

    #[test]
    fn test_axis() {
        // Operator 0: Axis
        produces("[[19 42] [0 3] 0 2]", "[42 19]");
        produces("[[19 42] 0 3]", "42");
        produces("[[[97 2] [1 42 0]] 0 7]", "[42 0]");

        // Bignum axis.
        produces("[[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 \
                  23 24 25 26 27 28 29 30 31 32 33] 0 8589934591]",
                 "33");
    }

    #[test]
    fn test_just() {
        // Operator 1: Just
        produces("[42 1 57]", "57");
    }

    #[test]
    fn test_fire() {
        // Operator 2: Fire
        produces("[[[40 43] [4 0 1]] [2 [0 4] [0 3]]]", "41");
        produces("[[[40 43] [4 0 1]] [2 [0 5] [0 3]]]", "44");
        produces("[77 [2 [1 42] [1 1 153 218]]]", "[153 218]");
    }

    #[test]
    fn test_depth() {
        // Operator 3: Depth
        produces("[1 3 0 1]", "1");
        produces("[[2 3] 3 0 1]", "0");
    }

    #[test]
    fn test_bump() {
        // Operator 4: Bump
        produces("[57 4 0 1]", "58");
    }

    #[test]
    fn test_bigint() {
        // 32-bit limit, bump up needs bignums if atom is u32
        produces("[4294967295 4 0 1]", "4.294.967.296");
        // 64-bit limit, bump up needs bignums if atom is u64
        produces("[18446744073709551615 4 0 1]", "18.446.744.073.709.551.616");
        // Bignum-to-bignum bump works, even if base atoms are 64-bit.
        produces("[18446744073709551616 4 0 1]", "18.446.744.073.709.551.617");
    }

    #[test]
    fn test_same() {
        // Operator 5: Same
        produces("[[1 1] 5 0 1]", "0");
        produces("[[1 2] 5 0 1]", "1");
        // Various bignum combinations.
        produces("[[18446744073709551615 18446744073709551615] 5 0 1]", "0");
        produces("[[18446744073709551615 18446744073709551616] 5 0 1]", "1");
        produces("[[18446744073709551615 2] 5 0 1]", "1");
        produces("[[2 18446744073709551615] 5 0 1]", "1");
    }

    #[test]
    fn test_if() {
        // Operator 6: If
        produces("[[40 43] 6 [3 0 1] [4 0 2] [4 0 1]]", "41");
        produces("[42 6 [1 0] [4 0 1] 1 233]", "43");
        produces("[42 6 [1 1] [4 0 1] 1 233]", "233");
    }

    #[test]
    fn test_misc_nock() {
        // Operator 7: Compose
        produces("[[42 44] [7 [4 0 3] [3 0 1]]]", "1");

        // Operator 8: Push

        // Operator 9: Call

        // Operator 10: Hint

        produces("[[132 19] [10 37 [4 0 3]]]", "20");

        // Fibonacci numbers,
        // https://groups.google.com/forum/#!topic/urbit-dev/K7QpBge30JI
        produces("[10 8 [1 1 1] 8 [1 0] 8 [1 6 [5 [0 15] 4 0 6] [0 28] 9 2 \
                  [0 2] [4 0 6] [[0 29] 7 [0 14] 8 [1 0] 8 [1 6 [5 [0 14] 0 \
                  6] [0 15] 9 2 [0 2] [4 0 6] [0 14] 4 0 15] 9 2 0 1] 0 15] \
                  9 2 0 1]",
                 "55");
    }

    #[test]
    fn test_stack() {
        // Subtraction. Tests tail call elimination, will trash stack if it
        // doesn't work.
        produces("[10.000 8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] \
                  [4 0 6] 0 7] 9 2 0 1]",
                 "9.999");
    }
}
