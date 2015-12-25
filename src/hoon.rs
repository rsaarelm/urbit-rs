use std::str;
use std::str::FromStr;
use nom::*;
use nom::IResult::*;
use nom::Err::*;
use nock::Noun;

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

// TODO: Handle separator dots
named!(atom<Noun>,
  map_res!(
    map_res!(
      digit,
      str::from_utf8
    ),
    FromStr::from_str
  )
);

named!(pqr<(Noun, Noun, Noun)>,
    alt!(
        chain!(
            p: preceded!(gap, atom) ~
            q: preceded!(gap, atom) ~
            r: preceded!(gap, atom),
            || { (p, q, r) })
        | delimited!(
            tag!("("),
            chain!(
                p: atom ~
                multispace ~
                q: atom ~
                multispace ~
                r: atom,
                || { (p, q, r) }),
            tag!(")"))
    )
);

named!(wtcl<Noun>,
   chain!(
       tag!("?:")
     ~ a: pqr
     , || { let (p, q, r) = a.clone(); n![6, p, q, r] }
    )
);

named!(hoon<Noun>,
    alt!(
        atom
      | wtcl
    )
);


#[cfg(test)]
mod test {
    use std::rc::Rc;
    use nom::IResult;
    use nock::Noun;
    use super::{gap, atom, pqr, wtcl, hoon};

    #[test]
    fn test_parse_atom() {
        assert_eq!(atom(&b"1234"[..]),
                   IResult::Done(&b""[..], Noun::Atom(1234)));
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

    #[test]
    fn test_parse_hoon() {
        assert_eq!(pqr(&b"(1 2 3)"[..]),
                   IResult::Done(&b""[..], (Noun::Atom(1), Noun::Atom(2), Noun::Atom(3))));
        assert_eq!(pqr(&b"\n1\n2\n3"[..]),
                   IResult::Done(&b""[..], (Noun::Atom(1), Noun::Atom(2), Noun::Atom(3))));
        assert_eq!(pqr(&b"  1  2  3"[..]),
                   IResult::Done(&b""[..], (Noun::Atom(1), Noun::Atom(2), Noun::Atom(3))));

        assert_eq!(wtcl(&b"?:(1 2 3)"[..]),
                   IResult::Done(&b""[..], n![6, 1, 2, 3]));
        assert_eq!(wtcl(&b"?:(1   2\n3)"[..]),
                   IResult::Done(&b""[..], n![6, 1, 2, 3]));
        assert_eq!(wtcl(&b"?:  1\n  2\n3"[..]),
                   IResult::Done(&b""[..], n![6, 1, 2, 3]));
    }

    #[test]
    fn test_decrement() {
        // Simple test case that needs quite a bit of machinery up and
        // running.
        let code = b"\
            =>  a=.                     ::  line 1
            =+  b=0                     ::  line 2
            |-                          ::  line 3
            ?:  =(a +(b))               ::  line 4
              b                         ::  line 5
            $(b +(b))                   ::  line 6";
        if let IResult::Done(_, noun) = hoon(&code[..]) {
            assert_eq!(n![42, noun].nock(),
                Ok(Rc::new(Noun::Atom(41))));
        } else {
            panic!("Failed to parse decrement code");
        }
    }
}
