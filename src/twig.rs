use num::bigint::BigUint;
use nom::IResult;
use nock::Noun;
use ream::ream;

/// Nock atom, an arbitrary-size unsigned integer.
pub type Atom = BigUint;
/// A Hoon symbol.
pub type Term = String;
/// A path of names.
pub type Wing = Vec<String>;
/// A list of bindings.
pub type Tram = Vec<(Wing, Box<Twig>)>;

/// Rust-native representation for Hoon's abstract syntax trees.
///
/// See ++twig in Urbit's hoon.hoon for reference.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Twig {
    Cnts(Wing, Tram), // eval p with changes from q

    Dtzy(Term, Atom), // atom constant

    Ktts(Term, Box<Twig>), // toga

    Tsls(Box<Twig>, Box<Twig>), // push p on subject of q

    Wtcl(Box<Twig>, Box<Twig>, Box<Twig>), // if-then-else
}

use Twig::*;

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
            &Dtzy(_, ref atom) => Ok(n![1, atom.clone()]),

            // TODO
            _ => Err(CompileError::CompileError),
        }
    }
}

pub fn make(input: &[u8]) -> Result<Noun, CompileError> {
    if let IResult::Done(_, twig) = ream(input) {
        twig.ut()
    } else {
        Err(CompileError::ParseError)
    }
}


#[cfg(test)]
mod test {
    use std::str;
    use std::rc::Rc;
    use nom::IResult;
    use nock::Noun;
    use ream::ream;

    fn evals(input: &str, output: &str) {
        if let IResult::Done(tail, twig) = ream(input.as_bytes()) {
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
