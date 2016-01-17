extern crate urbit;

use std::str;
use urbit::{ream, unpack_pill};

const HOON_SRC: &'static str = include_str!("assets/hoon.hoon");
const HOON_TWIG: &'static [u8] = include_bytes!("assets/hoontwig.pill");

fn lines(text: &str) -> usize {
    text.chars().filter(|&x| x == '\n').count()
}

#[test]
#[ignore] // TODO
fn test_parse_hoon_hoon() {
    let twig = unpack_pill(HOON_TWIG.to_vec()).unwrap();

    let parse = ream(HOON_SRC.as_bytes());
    if let Err(tail) = parse {
        let err_txt = str::from_utf8(tail).unwrap();
        println!("Parse error on line {}, starting from '{}'",
                 lines(HOON_SRC) - lines(err_txt) + 1,
                 err_txt.chars().take_while(|&x| x != '\n').collect::<String>());
        assert!(false);
    }
    assert!(parse.is_ok(), "Failed to parse hoon.hoon");

    let parse_noun = parse.unwrap().1.to_noun();
    assert!(parse_noun == twig, "Parse result does not match reference version");
}
