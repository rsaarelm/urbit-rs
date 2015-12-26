//! Parse hoon source into AST twig

use std::str;
use std::str::FromStr;
use nom::*;
use nom::IResult::*;
use nom::Err::*;

use twig::Twig;
use twig::Twig::*;

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
        return Error(Position(ErrorKind::MultiSpace, input));
    } else {
        return Done(&input[idx..], &input[0..idx]);
    }
}

/// Parse a Hoon expression into an AST.
named!(pub ream<Twig>,
    alt!(
        atom

        // TODO: Rest of hoon
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
    |x| Ok::<Twig, ()>(Dtzy("ud".to_string(), x))
  )
);


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
