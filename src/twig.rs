#![allow(non_camel_case_types)]

use std::str;
use num::bigint::BigUint;
use nock::Noun;
use ream::ream;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Tile,
    Wing,
    Base,
    Term,
    Toga,
    Chum,
    Tiki,
    Tine,
    Twig,
    Spot,
    Tyre,

    // Ones below need special parsing.
    Map,
    Tusk,
    Tram,
}

impl Type {
    /// Return whether this type can be matched with a regular twig parser.
    pub fn is_regular(self) -> bool {
        use self::Type::*;
        match self {
            Map | Tusk | Tram => false,
            _ => true,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct RuneData {
    pub rune: Rune,
    pub name: &'static str,
    arg: RuneArgs,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct RuneArgs {
    // Arity 4 ought to be enough for anybody.
    p: Option<Type>,
    q: Option<Type>,
    r: Option<Type>,
    s: Option<Type>,
}

macro_rules! runes {
    {$count: expr, $([$name:ident, $args:expr],)*} => {
        pub static RUNES: [RuneData; $count] = [
            $(RuneData { rune: Rune::$name, name: stringify!($name), arg: $args}),*
        ];
        #[derive(Copy, Clone, PartialEq, Eq, Debug)]
        pub enum Rune {
            $($name),*
        }

        impl Rune {
            pub fn glyph(self) -> Option<String> {
                // XXX: Doing a lot of work at runtime for what's basically a
                // static lookup. Could use stronger compile-time programming
                // here. (Or just a hardcoded look-up table.)
                let name = RUNES[self as usize].name;
                let mut ret = String::new();
                for i in [&name.as_bytes()[..2], &name.as_bytes()[2..]].iter() {
                    let s = str::from_utf8(i).expect("Non-ASCII7 rune name?");
                    if let Some(glyph) = cc_to_glyph(s) {
                        ret.push(glyph)
                    } else {
                        return None;
                    }
                }
                return Some(ret);

                fn cc_to_glyph(cc: &str) -> Option<char> {
                    match cc {
                        "br" => Some('|'),
                        "bs" => Some('\\'),
                        "bc" => Some('$'),
                        "cb" => Some('_'),
                        "cn" => Some('%'),
                        "cl" => Some(':'),
                        "cm" => Some(','),
                        "dq" => Some('"'),
                        "dt" => Some('.'),
                        "fs" => Some('/'),
                        "gl" => Some('<'),
                        "gr" => Some('>'),
                        "hx" => Some('#'),
                        "hp" => Some('-'),
                        "kl" => Some('{'),
                        "kr" => Some('}'),
                        "kt" => Some('^'),
                        "ls" => Some('+'),
                        "pm" => Some('&'),
                        "pt" => Some('@'),
                        "pl" => Some('('),
                        "pr" => Some(')'),
                        "sl" => Some('['),
                        "sm" => Some(';'),
                        "sr" => Some(']'),
                        "sg" => Some('~'),
                        "sq" => Some('\''),
                        "tr" => Some('*'),
                        "tc" => Some('`'),
                        "ts" => Some('='),
                        "wt" => Some('?'),
                        "zp" => Some('!'),
                        _ => None
                    }
                }
            }

            /// Return the number of arguments this rune takes
            pub fn arity(self) -> usize {
                if RUNES[self as usize].arg.s.is_some() { return 4; }
                if RUNES[self as usize].arg.r.is_some() { return 3; }
                if RUNES[self as usize].arg.q.is_some() { return 2; }
                if RUNES[self as usize].arg.p.is_some() { return 1; }
                return 0;
            }

            /// Return a vector of the types the rune takes.
            pub fn args(self) -> Vec<Type> {
                let mut ret = Vec::new();
                if let Some(a) = RUNES[self as usize].arg.p { ret.push(a); } else { return ret; }
                if let Some(a) = RUNES[self as usize].arg.q { ret.push(a); } else { return ret; }
                if let Some(a) = RUNES[self as usize].arg.r { ret.push(a); } else { return ret; }
                if let Some(a) = RUNES[self as usize].arg.s { ret.push(a); }
                return ret;
            }

            /// Return whether this rune can have a regular parser.
            pub fn is_regular(self) -> bool {
                if RUNES[self as usize].arg.p.map_or(false, |x| !x.is_regular()) { return false; }
                if RUNES[self as usize].arg.q.map_or(false, |x| !x.is_regular()) { return false; }
                if RUNES[self as usize].arg.r.map_or(false, |x| !x.is_regular()) { return false; }
                if RUNES[self as usize].arg.s.map_or(false, |x| !x.is_regular()) { return false; }
                return true;
            }
        }
    }
}

macro_rules! a {
    [] => ( RuneArgs { p: None, q: None, r: None, s: None } );
    [$p:ident] => ( RuneArgs { p: Some(Type::$p), q: None, r: None, s: None } );
    [$p:ident, $q:ident] => ( RuneArgs { p: Some(Type::$p), q: Some(Type::$q), r: None, s: None } );
    [$p:ident, $q:ident, $r:ident] => ( RuneArgs { p: Some(Type::$p), q: Some(Type::$q), r: Some(Type::$r), s: None } );
    [$p:ident, $q:ident, $r:ident, $s:ident] => ( RuneArgs { p: Some(Type::$p), q: Some(Type::$q), r: Some(Type::$r), s: Some(Type::$s) } );
}

runes! {
    112, // Rune count. Yes, we still need to do this in Rust if we want a static data array...

    // Pull stuff out of ++twig in hoon.hoon and mangle for Rust:

                                                        //////  tiling
    [bccm, a![Tile]],                                   //  clam a tile
    [bcpt, a![Wing, Tile]],                             //  whip p into q
    [bctr, a![Tile]],                                   //  bunt a tile w/ ^~
    [bczp, a![Base]],                                   //  bunt an axil
                                                        //////  cores
    [brcb, a![Tile, Map]],                              //  %gold tray, sample p
    [brcn, a![Map]],                                    //  %gold core, natural
    [brdt, a![Twig]],                                   //  dry %gold trap
    [brfs, a![Tile, Map]],                              //  vulcan. %gold tray
    [brkt, a![Twig, Map]],                              //  %gold book
    [brhp, a![Twig]],                                   //  kick dry %gold trap
    [brls, a![Tile, Twig]],                             //  dry %iron gate
    [brpt, a![Tile, Tile, Twig]],                       //  XX not used
    [brtr, a![Tile, Twig]],                             //  vulcan. wet gate
    [brts, a![Tile, Twig]],                             //  dry %gold gate
    [brwt, a![Twig]],                                   //  dry %lead trap
                                                        //////  tuples
    [clcb, a![Twig, Twig]],                             //  [q p]
    [clcn, a![Tusk]],                                   //  [[p ~] ~]
    [clfs, a![Twig]],                                   //  [%$ [%$ p ~] ~]
    [clkt, a![Twig, Twig, Twig, Twig]],                 //  [p q r s]
    [clhp, a![Twig, Twig]],                             //  [p q]
    [clls, a![Twig, Twig, Twig]],                       //  [p q r]
    [clsg, a![Tusk]],                                   //  [p ~]
    [cltr, a![Tusk]],                                   //  p as a tuple
    [clzz, a![Tusk]],                                   //  macro
                                                        //////  invocations
    [cncb, a![Wing, Tram]],                             //  %=, then cast to p
    [cncl, a![Twig, Twig]],                             //  pull $.p w/ sample q
    [cndt, a![Twig, Twig]],                             //  %-(q p)
    [cnhp, a![Twig, Tusk]],                             //  slam p w/ sample q
    [cntr, a![Wing, Twig, Tram]],                       //  pull p.q w/ changes
    [cnkt, a![Twig, Twig, Twig, Twig]],                 //  slam p w/ :*(q r s)
    [cnls, a![Twig, Twig, Twig]],                       //  slam p w/ :*(q r)
    [cnsg, a![Wing, Twig, Twig]],                       //  pull p from q with r
    [cnts, a![Wing, Tram]],                             //  eval. p w/ q changes
    [cnzy, a![Term]],                                   //  pulls limb p
    [cnzz, a![Wing]],                                   //  pulls p
                                                        //////  nock
    [dtkt, a![Twig]],                                   //  nock 11 data skyhook
    [dtls, a![Twig]],                                   //  nock 4 increment
    [dtzy, a![Term, Twig]],                             //  atom constant
    [dtzz, a![Term, Twig]],                             //  cubical constant
    [dttr, a![Twig, Twig]],                             //  nock p w/ formula q
    [dtts, a![Twig, Twig]],                             //  nock 5 equality test
    [dtwt, a![Twig]],                                   //  nock 3 cell test
                                                        //////  prettyprinting
    [hxgl, a![Tusk]],                                   //  prettyprint tape
    [hxgr, a![Tusk]],                                   //  prettyprint tank
                                                        //////  type conversion
    [ktbr, a![Twig]],                                   //  %gold core to %iron
    [ktdt, a![Twig, Twig]],                             //  cast q to type (p q)
    [ktls, a![Twig, Twig]],                             //  cast q to p, verify
    [kthx, a![Twig, Twig]],                             //  cast q to p, verify
    [kthp, a![Tile, Twig]],                             //  cast q to icon of p
    [ktpm, a![Twig]],                                   //  %gold core to %zinc
    [ktsg, a![Twig]],                                   //  p as static constant
    [ktts, a![Toga, Twig]],                             //  wrap q in toga p
    [ktwt, a![Twig]],                                   //  %gold core to %lead
                                                        //////  hints
    [sgbr, a![Twig, Twig]],                             //  print p if q fails
    [sgcb, a![Twig, Twig]],                             //  put p in q's trace
    [sgcn, a![Chum, Twig, Tyre, Twig]],                 //  mark core for jets
    [sgfs, a![Chum, Twig]],                             //  jet arm in ~% core
    [sggl, a![Twig, Twig]],                             //  hint p to product q
    [sggr, a![Twig, Twig]],                             //  hint p to q
    [sgbc, a![Term, Twig]],                             //  label q, profiling
    [sgls, a![Twig, Twig]],                             //  cache/memoize
    [sgpm, a![Twig, Twig, Twig]],                       //  print q w/priority
    [sgts, a![Twig, Twig]],                             //  avoid duplication
    [sgwt, a![Twig, Twig, Twig, Twig]],                 //  hint iff q is yes
    [sgzp, a![Twig, Twig]],                             //  type in stacktrace
                                                        //////  miscellaneous
    [smcl, a![Twig, Tusk]],                             //  binary to n-ary
    [smdt, a![Twig, Tusk]],                             //
    [smdq, a![Twig]],                                   //  assemble string
    [smsg, a![Twig, Tusk]],                             //  gonads
    [smsm, a![Tile, Twig]],                             //  make sure q is a p
                                                        //////  compositions
    [tsbr, a![Tile, Twig]],                             //  push bunt: =+(*p q)
    [tscl, a![Tram, Twig]],                             //  p changes, then q
    [tscn, a![Twig, Twig]],                             //  XX not used
    [tsdt, a![Wing, Twig, Twig]],                       //  r with p set to q
    [tsfs, a![Twig, Twig]],                             //  XX not used
    [tsgl, a![Twig, Twig]],                             //  =>(q p)
    [tshp, a![Twig, Twig]],                             //  flip push: =+(q p)
    [tsgr, a![Twig, Twig]],                             //  use p as .. of q
    [tskt, a![Twig, Twig, Twig, Twig]],                 //  state machine wing
    [tsls, a![Twig, Twig]],                             //  push p on .. of q
    [tspm, a![Tile, Twig]],                             //  XX not used
    [tspt, a![Tile, Twig]],                             //  XX not used
    [tstr, a![Term, Wing, Twig]],                       //  make a %bull/alias
    [tssg, a![Tusk]],                                   //  compose twig list
                                                        //////  conditionals
    [wtbr, a![Tusk]],                                   //  logical OR
    [wthp, a![Wing, Tine]],                             //  select case in q
    [wthz, a![Tiki, Tine]],                             //  tiki %wthp
    [wtcl, a![Twig, Twig, Twig]],                       //  if p, then q, else r
    [wtdt, a![Twig, Twig, Twig]],                       //  unless, ?:(p r q)
    [wtkt, a![Wing, Twig, Twig]],                       //  if p is a cell
    [wtkz, a![Tiki, Twig, Twig]],                       //  tiki %wtkt
    [wtgl, a![Twig, Twig]],                             //  assert |, ?:(p !! q)
    [wtgr, a![Twig, Twig]],                             //  assert &, ?:(p q !!)
    [wtls, a![Wing, Twig, Tine]],                       //  %wthp w/ default
    [wtlz, a![Tiki, Twig, Tine]],                       //  tiki %wtls
    [wtpm, a![Tusk]],                                   //  logical AND
    [wtpt, a![Wing, Twig, Twig]],                       //  if p is an atom
    [wtpz, a![Tiki, Twig, Twig]],                       //  tiki %wtpt
    [wtsg, a![Wing, Twig, Twig]],                       //  if p is null
    [wtsz, a![Tiki, Twig, Twig]],                       //  tiki %wtsg
    [wtts, a![Tile, Wing]],                             //  if q is in tile p
    [wttz, a![Tile, Tiki]],                             //  tiki %wtts
    [wtzp, a![Twig]],                                   //  logical NOT
                                                        //////  special
    [zpcb, a![Spot, Twig]],                             //  debug info in trace
    [zpcm, a![Twig, Twig]],                             //  q twig with p type
    [zpcn, a![]],                                       //  obsolete
    [zpfs, a![Twig]],                                   //  report .. as error
    [zpgr, a![Twig]],                                   //  vase w/ value p
    [zpsm, a![Twig, Twig]],                             //  [type noun] pair
    [zpts, a![Twig]],                                   //  Nock formula of p
    [zpwt, a![Twig]],                                   //  restrict hoon vers.
    [zpzp, a![]],                                       //  always crash
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Odor {
    // Unsigned integer.
    ud, // TODO: The rest
}

/// Rust-native representation for Hoon's abstract syntax trees.
///
/// See ++twig in Urbit's hoon.hoon for reference.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Twig {
    Cell(Box<Twig>, Box<Twig>),
    Rune(Rune),
    Atom(Odor, BigUint),
    // Wing parts are usually term names, but '.' is a raw Nock [0 1]. Treat
    // them all as nock, assume atoms are terms and cells are formulas.
    Wing(Vec<Noun>),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum CompileError {
    ParseError,
    CompileError,
}

// XXX: Will probably need to add a context object to ut to handle symbol
// lookup.

impl Twig {
    /// Compile a twig into a Nock formula.
    pub fn ut(&self) -> Result<Noun, CompileError> {

        match self {
            // Constant.
            &Twig::Cell(box Twig::Rune(Rune::dtzy), box Twig::Atom(_, ref atom)) => {
                Ok(n![1, atom.clone()])
            }

            // TODO
            _ => Err(CompileError::CompileError),
        }
    }
}

pub fn make(input: &[u8]) -> Result<Noun, CompileError> {
    if let Ok((_, twig)) = ream(input) {
        twig.ut()
    } else {
        Err(CompileError::ParseError)
    }
}


#[cfg(test)]
mod test {
    use std::str;
    use std::rc::Rc;
    use nock::Noun;
    use ream::ream;

    fn evals(input: &str, output: &str) {
        match ream(input.as_bytes()) {
            Ok((tail, twig)) => {
                let formula = twig.ut().ok().expect("Compile failed");
                // Nock the formula against an empty subject.
                // Might want to allow passing a subject in evals interface.
                let result = Noun::Cell(Rc::new(Noun::Atom(0)), Rc::new(formula))
                                 .nock()
                                 .ok()
                                 .expect("Eval failed");

                assert_eq!(format!("{}", result), output);
                assert!(tail == &b""[..],
                        format!("Unparsed suffix left in input: '{}'",
                                str::from_utf8(tail).unwrap()));
            }
            Err(e) => panic!("Parsing failed after {}", str::from_utf8(e).unwrap()),
        }
    }

    #[test]
    fn test_rune() {
        use super::Rune::*;

        assert_eq!(tsgr.arity(), 2);
        assert_eq!(&tsgr.glyph().unwrap(), "=>");

        assert!(tsgr.is_regular());
        assert!(wtcl.is_regular());
        assert!(zpzp.is_regular());
        assert!(!cntr.is_regular());
        assert!(!cltr.is_regular());
    }

    #[test]
    fn test_atom() {
        evals("0", "0");
        evals("123", "123");
        evals("  123", "123");
        evals(" :: IGNORE ME\n  123", "123");
    }

    #[test]
    fn test_simplified_decrement() {
        // Simplified version that doesn't use irregular runes.
        evals(
           "=>  42
            =>  ^=(a .)
            =+  ^=(b 0)
            |-
            ?:  .=(a .+(b))
              b
            %=($ b .+(b))",

            "41"
            );
    }

    #[test]
    fn test_decrement() {
        evals(
           "=>  42
            =>  a=.
            =+  b=0
            |-
            ?:  =(a +(b))
              b
            $(b +(b))",

            "41"
            );
    }
}
