#![allow(non_camel_case_types)]

use num::bigint::BigUint;
use nock::Noun;
use ream::ream;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Rune {
    brhp, // kicked dry trap
    dtls, // Nock increment
    dtts, // Nock equality
    cnts, // eval p with changes from q
    dtzy, // atom constant
    ktts, // wrap value in toga
    tsgr, // set p as subject of q
    tsls, // push p on subject of q
    wtcl, // if-then-else

    // TODO: Fill out the rest.
}

impl Rune {
    pub fn glyph(self) -> &'static str {
        use self::Rune::*;
        match self {
            brhp => "|-",
            dtls => ".+",
            dtts => ".=",
            cnts => "%=",
            dtzy => panic!("Internal rune"),
            ktts => "^=",
            tsgr => "=>",
            tsls => "=+",
            wtcl => "?:",
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Odor {
    ud, // unsigned integer

    // TODO: Fill out the rest.
}

/// Rust-native representation for Hoon's abstract syntax trees.
///
/// See ++twig in Urbit's hoon.hoon for reference.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Twig {
    Cell(Box<Twig>, Box<Twig>),
    Rune(Rune),
    Atom(Odor, BigUint),
    Wing(Vec<String>),
}

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
            &Twig::Cell(box Twig::Rune(Rune::dtzy), box Twig::Atom(_, ref atom)) =>
                Ok(n![1, atom.clone()]),

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
        if let Ok((tail, twig)) = ream(input.as_bytes()) {
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
        } else {
            panic!("Parse error");
        }
    }

    #[test]
    fn test_atom() {
        evals("0", "0");
        evals("123", "123");
    }

    #[test]
    fn test_simplified_decrement() {
        // Simplified version that doesn't use irregular runes.
        evals(
           "=>  42
            =>  ^=(a .)
            =+  b=0
            |-
            ?:  .=(a +(b))
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
