use std::error::Error;
use std::fmt;
use std::hash;
use std::iter::FromIterator;
use std::collections::HashMap;
use nock::{self, Noun, FromNoun, ToNoun, NockError, NockResult};
use jets;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct JetError(pub String);

pub type JetResult<T> = Result<T, JetError>;

impl fmt::Display for JetError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for JetError {
    fn description(&self) -> &str {
        &self.0[..]
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

pub struct Jet {
    pub name: String,
    pub jet: Option<fn(&Noun) -> NockResult>,
    pub battery: Noun,
    // TODO: What do we do with these?
    pub axis: u32,
    pub hooks: HashMap<String, Noun>,
    pub calls: u64,
}

impl Jet {
    pub fn new(name: String, battery: Noun, axis: u32, hooks: Vec<(String, Noun)>) -> Jet {
        let jet = match_jet(&name[..]);
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

/// Wrap a single-argument function as a jet.
///
/// The argument will be extracted from axis 6 of subject.
#[inline]
fn wrap1<T1, U, F>(jet_name: &str, f: F, subject: &Noun) -> NockResult
where T1: FromNoun,
      U: ToNoun,
      F: Fn(T1) -> JetResult<U> {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();
    let t1: T1 = try!(FromNoun::from_noun(&arg));
    let ret = try!(f(t1).map_err(|e| NockError(format!("jet {} {}", jet_name, e))));
    Ok(ret.to_noun())
}

#[inline]
fn wrap2<T1, T2, U, F>(jet_name: &str, f: F, subject: &Noun) -> NockResult
where T1: FromNoun,
      T2: FromNoun,
      U: ToNoun,
      F: Fn(T1, T2) -> JetResult<U> {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();
    let (t1, t2): (T1, T2) = try!(FromNoun::from_noun(&arg));
    let ret = try!(f(t1, t2).map_err(|e| NockError(format!("jet {} {}", jet_name, e))));
    Ok(ret.to_noun())
}

#[inline]
fn wrap3<T1, T2, T3, U, F>(jet_name: &str, f: F, subject: &Noun) -> NockResult
where T1: FromNoun,
      T2: FromNoun,
      T3: FromNoun,
      U: ToNoun,
      F: Fn(T1, T2, T3) -> JetResult<U> {
    let arg = nock::get_axis(&Noun::from(6u32), subject).unwrap();
    let (t1, t2, t3): (T1, T2, T3) = try!(FromNoun::from_noun(&arg));
    let ret = try!(f(t1, t2, t3).map_err(|e| NockError(format!("jet {} {}", jet_name, e))));
    Ok(ret.to_noun())
}

/*
/// Give jet full access to the subject.
#[inline]
fn wrap_raw<U, F>(jet_name: &str, f: F, subject: &Noun) -> NockResult
where U: ToNoun,
      F: Fn(&Noun) -> JetResult<U> {
    let ret = try!(f(subject).map_err(|e| NockError(format!("jet {} {}", jet_name, e))));
    Ok(ret.to_noun())
}
*/

/// Construct a jet matcher from tuples of (internal_jet_function, urbit_jet_name, jet_wrapper_function).
macro_rules! match_jet{
    ($($f:ident: $name:expr, $wrap_fn:ident;)*) => {
        $(fn $f(subject: &Noun) -> NockResult { $wrap_fn($name, jets::$f, subject) })*

        fn match_jet(name: &str) -> Option<fn(&Noun) -> NockResult> {
            match name {
                $($name => Some($f as fn(&Noun) -> NockResult),)*
                _ => None
            }
        }
    }
}

match_jet!{
    dec:  "dec",  wrap1;
    bex:  "bex",  wrap1;
    add:  "add",  wrap2;
    mul:  "mul",  wrap2;
    sub:  "sub",  wrap2;
    div:  "div",  wrap2;
    lth:  "lth",  wrap2;
    rsh:  "rsh",  wrap3;
    lsh:  "lsh",  wrap3;
    mod_: "mod",  wrap2;
    mix:  "mix",  wrap2;
    met:  "met",  wrap2;
    mug:  "mug",  wrap1;
    end:  "end",  wrap3;
    cut:  "cut",  wrap3;
}
