use std::str;
use std::str::FromStr;
use nom::*;
use nom::IResult::*;
use nock::Noun;

// XXX: Is there a nicer way to ignore the results from eg. newline than
// the superfluous chain!?
named!(gap<()>, alt!(
        chain!(newline, || { () })
      | chain!(tag!(" ") ~ tag!(" ") ~ multispace, || { () })
    )
);

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

named!(wtcl<Noun>,
   chain!(
       tag!("?:") ~
       x: alt!(
           chain!(
               gap ~
               p: atom ~
               gap ~
               q: atom ~
               gap ~
               r: atom,
               || { n![6, p, q, r] })
        | delimited!(
            tag!("("),
            chain!(
               p: atom ~
               space ~
               q: atom ~
               space ~
               r: atom,
               || { n![6, p, q, r] }),
            tag!(")"))
        ),
    || { x }
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
    use nom::IResult;
    use nock::Noun;
    use super::{gap, atom, wtcl};

    #[test]
    fn test_parse_hoon() {
        assert_eq!(atom(&b"1234"[..]), IResult::Done(&b""[..], Noun::Atom(1234)));

        assert_eq!(gap(&b"  "[..]), IResult::Done(&b""[..], ()));
        assert_eq!(gap(&b"    "[..]), IResult::Done(&b""[..], ()));
        assert_eq!(gap(&b"\n"[..]), IResult::Done(&b""[..], ()));
        assert!(gap(&b" "[..]) != IResult::Done(&b""[..], ()));

        assert_eq!(wtcl(&b"?:(1 2 3)"[..]), IResult::Done(&b""[..], n![6, 1, 2, 3]));
        assert_eq!(wtcl(&b"?:  1\n  2\n3"[..]), IResult::Done(&b""[..], n![6, 1, 2, 3]));
    }
}
