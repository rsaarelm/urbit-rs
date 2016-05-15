#![feature(alloc_system)]

extern crate alloc_system;
extern crate num;
extern crate fnv;
#[macro_use]
extern crate nock;

use std::rc::Rc;
use std::collections::HashMap;
use nock::{Nock, Noun, NockError, NockResult, Shape, FromNoun};
use jet::Jet;

mod jet;
mod jets;

/// An Urbit virtual machine.
pub struct VM {
    jets: HashMap<Noun, jet::Jet>,
}

impl Nock for VM {
    fn call(&mut self, subject: &Noun, formula: &Noun) -> Option<NockResult> {
        if let Some(jet) = self.jets.get_mut(formula) {
            jet.calls += 1;

            if let Some(f) = jet.jet {
                return Some(f(subject));
            }
        }
        None
    }

    fn hint(&mut self, subject: &Noun, hint: &Noun, c: &Noun) -> Result<(), NockError> {
        let (id, clue) = match hint.get() {
            Shape::Cell(ref p, ref q) => {
                (*p, try!(self.nock_on((*subject).clone(), (*q).clone())))
            }
            Shape::Atom(_) => (hint, Noun::from(0u32)),
        };

        // TODO: Handle other hint types than %fast.
        if String::from_noun(id).unwrap() == "fast" {
            let core = try!(self.nock_on((*subject).clone(), (*c).clone()));
            if let Ok((name, axis, hooks)) = parse_fast_clue(&clue) {
                if let Shape::Cell(ref battery, _) = core.get() {
                    if !self.jets.contains_key(battery) {
                        let jet = Jet::new(name, (*battery).clone(), axis, hooks);
                        self.jets.insert((*battery).clone(), jet);
                    }
                } else {
                    return Err(NockError(format!("hint")));
                }
            } else {
                // println!("Unparseable clue...");
            }
        }
        Ok(())
    }
}

impl VM {
    pub fn new() -> VM {
        VM { jets: HashMap::new() }
    }

    pub fn print_status(&self) {
        let mut total_count = 0;
        let mut jets: Vec<&Jet> = self.jets.iter().map(|(_, x)| x).collect();
        jets.sort_by(|a, b| b.calls.cmp(&a.calls));
        for jet in &jets {
            if jet.calls < 100 || (jet.calls as f32) / (total_count as f32) < 1e-6 {
                // Don't care about the little things
                println!(" ...");
                break;
            }
            println!("{}{} called {} times",
                     if jet.jet.is_some() {
                         '*'
                     } else {
                         ' '
                     },
                     jet.name,
                     jet.calls);
            total_count += jet.calls;
        }
    }
}

fn parse_fast_clue(clue: &Noun) -> Result<(String, u32, Vec<(String, Noun)>), NockError> {
    if let Some((ref name, ref axis_formula, ref hooks)) = clue.get_122() {
        let chum = try!(String::from_noun(name).map_err(|_| NockError(format!("hint"))));

        let axis = if let Shape::Cell(ref a, ref b) = axis_formula.get() {
            if let (Some(1), Some(0)) = (a.as_u32(), b.as_u32()) {
                0
            } else if let (Some(0), Some(axis)) = (a.as_u32(), b.as_u32()) {
                axis
            } else {
                return Err(NockError(format!("hint")));
            }
        } else {
            return Err(NockError(format!("hint")));
        };

        let hooks: Vec<(String, Noun)> = try!(FromNoun::from_noun(hooks)
                                                  .map_err(|_| NockError(format!("hint"))));

        Ok((chum, axis, hooks))
    } else {
        Err(NockError(format!("hint")))
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
pub fn unpack_pill(mut buf: Vec<u8>) -> jet::JetResult<Noun> {
    // Guarantee that the buffer is made of 32-bit chunks.
    while buf.len() % 4 != 0 {
        buf.push(0);
    }

    jets::cue(Rc::new(buf))
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
}
