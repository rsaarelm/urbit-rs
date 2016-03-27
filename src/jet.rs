use std::hash;
use std::iter::FromIterator;
use std::collections::HashMap;
use num::{BigUint, FromPrimitive, One};
use nock::{self, Shape, Noun, FromNoun};

pub struct Jet {
    pub name: String,
    pub jet: Option<fn(&Noun) -> Noun>,
    pub battery: Noun,
    // TODO: What do we do with these?
    pub axis: u32,
    pub hooks: HashMap<String, Noun>,
    pub calls: u64,
}

impl Jet {
    pub fn new(name: String, battery: Noun, axis: u32, hooks: Vec<(String, Noun)>) -> Jet {
        let jet = match &name[..] {
            "dec" => Some(dec as fn(&Noun) -> Noun),
            "bex" => Some(bex as fn(&Noun) -> Noun),
            "add" => Some(add as fn(&Noun) -> Noun),
            "mul" => Some(mul as fn(&Noun) -> Noun),
            "sub" => Some(sub as fn(&Noun) -> Noun),
            "div" => Some(div as fn(&Noun) -> Noun),
            "lth" => Some(lth as fn(&Noun) -> Noun),
            "rsh" => Some(rsh as fn(&Noun) -> Noun),
            "lsh" => Some(lsh as fn(&Noun) -> Noun),
            "mod" => Some(mod_ as fn(&Noun) -> Noun),
            "end" => Some(end as fn(&Noun) -> Noun),
            "mix" => Some(mix as fn(&Noun) -> Noun),
            "met" => Some(met as fn(&Noun) -> Noun),
            "cut" => Some(cut as fn(&Noun) -> Noun),
            "mug" => Some(mug as fn(&Noun) -> Noun),
            _ => None,
        };

        Jet {
            name: name,
            jet: jet,
            battery: battery,
            axis: axis,
            hooks: HashMap::from_iter(hooks.into_iter()),
            calls: 0,
        }
    }
}

impl hash::Hash for Jet {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.battery.hash(state);
    }
}

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

pub fn end(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (a, b, c): (usize, usize, BigUint) = FromNoun::from_noun(&arg).unwrap();

    Noun::from(c % BigUint::from_usize(1 << ((1 << a) * b)).unwrap())
}

pub fn mix(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (a, b): (BigUint, BigUint) = FromNoun::from_noun(&arg).unwrap();

    Noun::from(a ^ b)
}

pub fn met(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    if let Shape::Cell(ref bloq, ref b) = arg.get() {
        let bloq: usize = FromNoun::from_noun(bloq).unwrap();

        let mut bits;
        if let Shape::Atom(ref atom) = b.get() {
            bits = atom.len() * 8;
            if bits > 0 {
                let last = atom[atom.len() - 1];
                for i in (1..8).rev() {
                    if last & (1 << i) == 0 {
                        bits -= 1;
                    } else {
                        break;
                    }
                }
            }
        } else {
            panic!("Bad atom");
        }

        bits += (1 << bloq) - 1;

        Noun::from(bits / (1 << bloq))
    } else {
        panic!("Bad argument");
    }
}

pub fn cut(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();

    let (a, (mut b, mut c), mut d): (usize, (usize, usize), BigUint) = FromNoun::from_noun(&arg).unwrap();
    b = b << a;
    c = c << (1 << a);
    d = d >> b;
    d = d % BigUint::from_usize(c).unwrap();

    Noun::from(d)
}

pub fn mug(subject: &Noun) -> Noun {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();
    Noun::from(arg.mug())
}
