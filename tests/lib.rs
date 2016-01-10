extern crate urbit;

use urbit::{ream, unpack_pill};

#[test]
#[ignore] // FIXME
fn test_parse_hoon_hoon() {
    use std::fs::File;
    use std::io::prelude::*;

    let mut hoon_src = Vec::new();
    File::open("assets/hoon.hoon").unwrap().read_to_end(&mut hoon_src).unwrap();

    let mut twig_pill = Vec::new();
    File::open("assets/hoontwig.pill").unwrap().read_to_end(&mut twig_pill).unwrap();
    let twig = unpack_pill(twig_pill).unwrap();

    let parse = ream(&hoon_src);
    assert!(parse.is_ok(), "Failed to parse hoon.hoon");

    let parse_noun = parse.unwrap().1.to_noun();
    assert!(parse_noun == twig, "Parse result does not match reference version");
}
