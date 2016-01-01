//! Parse hoon source into AST twig

use std::str;
use std::str::FromStr;
use nom::*;
use nom::IResult::*;
use nom::Err::*;

use twig::{Twig, Rune, Odor};

#[inline]
pub fn is_lowercase(chr: u8) -> bool {
    chr >= 'a' as u8 && chr <= 'z' as u8
}

/// Match at least two spaces or one newline.
pub fn long_space(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut spaces = 0;
    let mut idx = 0;
    for item in input.iter() {
        if *item == '\t' as u8 {
            // Physical tabs are banned.
            // XXX: Should have own ErrorKind tag.
            return Error(Position(ErrorKind::MultiSpace, input));
        }

        if *item == '\n' as u8 {
            spaces += 2;
        } else if *item == ' ' as u8 {
            spaces += 1;
        }

        if *item != ' ' as u8 && *item != '\r' as u8 && *item != '\n' as u8 {
            break;
        }

        idx += 1;
    }

    if spaces < 2 {
        Error(Position(ErrorKind::MultiSpace, input))
    } else {
        Done(&input[idx..], &input[0..idx])
    }
}

pub fn ident(input:&[u8]) -> IResult<&[u8], &[u8]> {
    for (idx, item) in input.iter().enumerate() {
        if idx == 0 {
            // TODO: Should we only accept lowercase chars?
            if !is_alphabetic(*item) {
                return Error(Position(ErrorKind::Alpha, input))
            }
        } else {
            if !is_alphabetic(*item) && !is_digit(*item) && *item != '-' as u8 {
                return Done(&input[idx..], &input[0..idx])
            }
        }
    }
    Error(Position(ErrorKind::Alpha, input))
}

named!(comment<&[u8]>,
    chain!(
        tag!("::") ~
        x: take_until_and_consume!("\n"),
        || { x }
    )
);

// A valid gap is any sequence of long spaces and comments.
named!(gap< Vec<&[u8]> >,
    many1!(
        alt!(
            long_space
          | comment
        )
    )
);

// TODO: Handle separator dots
// TODO: Handle other odors than ud.
named!(ud<Twig>,
  map_res!(
    map_res!(
      map_res!(
        digit,
        str::from_utf8
      ),
      FromStr::from_str
    ),
    |x| Ok::<Twig, ()>(Twig::Cell(
            box Twig::Rune(Rune::dtzy),
            box Twig::Atom(Odor::ud, x)))
  )
);

// TODO: Don't have two different-named identifier parsers that differ just on
// having the from_utf8...
/// Parse an identifier name.
named!(id<&str>,
    map_res!(
        ident,
        str::from_utf8
    )
);

/// Terminator for an arbitrary-length tall rune.
named!(tall_terminator<()>,
    chain!(
        gap ~
        tag!("=="),
      || ()
    )
);


macro_rules! tall_rune_args {
    ($i: expr, $first: tt) => {
        chain!($i,
            x: $first ~
            gap,
         || { x }
         )
    };
    ($i: expr, $first: tt, $($rest: tt),+) => {
        chain!($i,
            x: $first ~
            gap ~
            xs: tall_rune_args!($($rest),*),
         || { Twig::Cell(box x, box xs) }
         )
    };
}

macro_rules! wide_rune_args {
    ($i: expr, $first: tt) => {
        chain!($i,
            x: $first ~
            tag!(")"),
         || { x }
         )
    };
    ($i: expr, $first: tt, $($rest: tt),+) => {
        chain!($i,
            x: $first ~
            tag!(" ") ~
            xs: wide_rune_args!($($rest),*),
         || { Twig::Cell(box x, box xs) }
         )
    };
}

/// A standard rune that may have either a wide or a tall form.
macro_rules! rune {
    ($i: expr, $name:ident, $($parser: tt),+) => {
        match chain!($i,
            tag!(Rune::$name.glyph()) ~
            args: alt!(
                chain!(
                    tag!("(") ~
                    args: wide_rune_args!($($parser),*),
                    || { args }
                ) |
                chain!(
                    gap ~
                    args: tall_rune_args!($($parser),*),
                    || { args }
                )
            ),
            || { Twig::Cell(box Twig::Rune(Rune::$name), box args) }
        ) {
            // Map incomplete parse to error so that failing rune parses
            // don't halt the alt branching.
            //
            // XXX: Do we really need all this boilerplate just to make alt!
            // in ream parser work right?
            ::nom::IResult::Done(i,o)     => ::nom::IResult::Done(i, o),
            ::nom::IResult::Error(e)      => ::nom::IResult::Error(e),
            ::nom::IResult::Incomplete(_) =>
                ::nom::IResult::Error(::nom::Err::Code(::nom::ErrorKind::Custom(0))),
        }
    }
}

/// Parse a Hoon expression into an AST.
named!(pub ream<Twig>,
    alt!(
        rune!(brhp, ream) |
        rune!(dtls, ream) |
        rune!(dtts, ream, ream) |
        rune!(ktts, ream, ream) | // TODO: First argument should be 'toga', name-like-thing.
        rune!(tsgr, ream, ream) |
        rune!(wtcl, ream, ream, ream) |
        ud

        // TODO: Rest of hoon
    )
);


#[cfg(test)]
mod test {
    use super::gap;

    #[test]
    fn test_parse_gap() {
        assert!(gap(&b"  "[..]).is_done());
        assert!(gap(&b"    "[..]).is_done());
        assert!(gap(&b"\n"[..]).is_done());
        assert!(gap(&b"\n  "[..]).is_done());
        assert!(gap(&b"  \n  "[..]).is_done());
        assert!(!gap(&b" "[..]).is_done());
    }
}
