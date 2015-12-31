//! Parse hoon source into AST twig

use std::str;
use std::str::FromStr;
use nom::*;
use nom::IResult::*;
use nom::Err::*;

use twig::{Twig, Wing};

/// Match at least two spaces or one newline.
pub fn gap(input: &[u8]) -> IResult<&[u8], &[u8]> {
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
        } else if is_space(*item) {
            spaces += 1;
        }

        if !is_space(*item) && *item != '\r' as u8 && *item != '\n' as u8 {
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

/// Parse a Hoon expression into an AST.
named!(pub ream<Twig>,
    alt!(
        atom
      | Brhp
      | Dtls
      | Dtts
      | Ktts
      | Tsgr
      | Tsls
      | Wtcl

        // TODO: Rest of hoon
    )
);

named!(comment<&[u8]>,
    chain!(
        tag!("::") ~
        x: take_until_and_consume!("\n"),
        || { x }
    )
);

// TODO: Handle separator dots
// TODO: Handle other odors than ud.
named!(atom<Twig>,
  map_res!(
    map_res!(
      map_res!(
        digit,
        str::from_utf8
      ),
      FromStr::from_str
    ),
    |x| Ok::<Twig, ()>(Twig::Dtzy("ud".to_string(), x))
  )
);

named!(id<&str>,
    map_res!(
        ident,
        str::from_utf8
    )
);

named!(p<(Box<Twig>)>,
    alt!(
        chain!(
            p: preceded!(gap, ream),
            || { Box::new(p) })
        | delimited!(
            tag!("("),
            chain!(
                p: ream,
                || { Box::new(p) }),
            tag!(")"))
    )
);

named!(pq<(Box<Twig>, Box<Twig>)>,
    alt!(
        chain!(
            p: preceded!(gap, ream) ~
            q: preceded!(gap, ream),
            || { (Box::new(p), Box::new(q)) })
        | delimited!(
            tag!("("),
            chain!(
                p: ream ~
                space ~
                q: ream,
                || { (Box::new(p), Box::new(q)) }),
            tag!(")"))
    )
);

named!(pqr<(Box<Twig>, Box<Twig>, Box<Twig>)>,
    alt!(
        chain!(
            p: preceded!(gap, ream) ~
            q: preceded!(gap, ream) ~
            r: preceded!(gap, ream),
            || { (Box::new(p), Box::new(q), Box::new(r)) })
        | delimited!(
            tag!("("),
            chain!(
                p: ream ~
                space ~
                q: ream ~
                space ~
                r: ream,
                || { (Box::new(p), Box::new(q), Box::new(r)) }),
            tag!(")"))
    )
);

/// Regular rune with 1 argument
macro_rules! rune1 {
    ($id:ident, $rune:expr) => {
        named!($id<Twig>,
           chain!(
               tag!($rune) ~
               p: p,
               || {
                 Twig::$id(p)
               }
            )
        );
    }
}

/// Regular rune with 2 arguments
macro_rules! rune2 {
    ($id:ident, $rune:expr) => {
        named!($id<Twig>,
           chain!(
               tag!($rune) ~
               a: pq,
               || {
                 let (p, q) = a.clone();
                 Twig::$id(p, q)
               }
            )
        );
    }
}

/// Regular rune with 3 arguments
macro_rules! rune3 {
    ($id:ident, $rune:expr) => {
        named!($id<Twig>,
           chain!(
               tag!($rune) ~
               a: pqr,
               || {
                 let (p, q, r) = a.clone();
                 Twig::$id(p, q, r)
               }
            )
        );
    }
}

named!(wing<Wing>,
    chain!(
        x: id ~
        mut xs: many0!(
            chain!(
                tag!(".") ~
                x: id,
                || { x.to_string() }
            )
        ),
        || {
            xs.insert(0, x.to_string());
            xs
        }
    )
);


rune1!(Brhp, "|-");
rune1!(Dtls, ".+");
rune2!(Dtts, ".=");
rune2!(Ktts, "^=");
rune2!(Tsgr, "=>");
rune2!(Tsls, "=+");
rune3!(Wtcl, "?:");

/*
named!(Cnts<Twig>,
    // TODO: Tall form
    delimited!(
        tag!("%=("),
        chain!(
            p: wing ~
            space ~
            q1: wing ~
            space ~
            q2: ream ~
            rs: many0!(
                chain!(
                    tag!(",") ~
                    space ~
                    r1: wing ~
                    space ~
                    r2: ream,
                || { (r1, r2) }
                )
            ),

            || {
                rs.insert(0, (q1, q2));
                Twig::Cnts(p, rs)
            }),
        tag!(")"))
);
*/

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use num::FromPrimitive;
    use num::bigint::BigUint;
    use nom::IResult;
    use twig::Twig;
    use super::{gap, atom, ream};

    #[test]
    fn test_parse_atom() {
        assert_eq!(ream(&b"1234"[..]),
                   IResult::Done(&b""[..],
                                 Twig::Dtzy("ud".to_string(), BigUint::from_u32(1234).unwrap())));
    }

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
