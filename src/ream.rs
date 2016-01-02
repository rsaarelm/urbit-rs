//! Parse hoon source into AST twig

use std::str;
use std::str::FromStr;
use num::bigint::BigUint;

use twig::{Twig, Rune, Odor};

pub type ParseResult<'a, T> = Result<(&'a [u8], T), &'a [u8]>;

/// Parse Hoon source code into a Twig AST.
///
/// Return trailing input after a successful parse along with the resulting
/// Twig. Return trailing input after the point of failed parse on error.
pub fn ream(mut input: &[u8]) -> ParseResult<Twig> {
    // Eat initial space
    loop {
        if let Ok((tail, _)) = space_or_comment(input) {
            input = tail;
        } else {
            break;
        }
    }
    // TODO
    //Err(input)
    atom(input)
}

fn atom(input: &[u8]) -> ParseResult<Twig> {
    if let Ok((tail, num)) = ud(input) {
        Ok((tail,
            Twig::Cell(
                box Twig::Rune(Rune::dtzy),
                box Twig::Atom(Odor::ud, num))))
    } else {
        Err(input)
    }
}

/// Unsigned decimal
fn ud(input: &[u8]) -> ParseResult<BigUint> {
    let mut idx = 0;
    let mut num = String::new();
    for c in input.iter() {
        if *c >= '0' as u8 && *c <= '9' as u8 {
            num.push(*c as char);
        }
        else if idx > 0 && *c == '.' as u8 {
            // Ignore separator dots.
            // We're lazy, so we let them show up in any pattern after the
            // initial digit instead of insisting that they group the digits
            // in sized groups.
        } else {
            if idx == 0 { return Err(input); }
            break;
        }
        idx += 1;
    }

    Ok((&input[idx..], FromStr::from_str(&num).unwrap()))
}

fn space_or_comment(input: &[u8]) -> ParseResult<()> {
    let mut tail = input;
    loop {
        if let Ok((t, _)) = comment(input) {
            tail = t
        } else {
            if tail.len() == 0 {
                break;
            } else if is_whitespace(tail[0]) {
                tail = &tail[1..];
            } else {
                break;
            }
        }
    }
    if tail == input {
        Err(input)
    } else {
        Ok((tail, ()))
    }
}

fn comment(input: &[u8]) -> ParseResult<()> {
    if starts_with(input, "::") {
        Ok((split_after(input, '\n' as u8).0, ()))
    } else {
        Err(input)
    }
}

fn is_whitespace(c: u8) -> bool {
    // Physical tabs not included. A physical tab in the code is a syntax
    // error.
    c == 0x10 || c == 0x13 || c == ' ' as u8
}

fn is_lowercase(c: u8) -> bool {
    c >= 'a' as u8 && c <= 'z' as u8
}

fn starts_with(input: &[u8], prefix: &str) -> bool {
    let prefix = prefix.as_bytes();
    if input.len() < prefix.len() {
        return false;
    }

    for (x, y) in prefix.iter().zip(input.iter()) {
        if x != y { return false; }
    }

    true
}

fn split_after(input: &[u8], c: u8) -> (&[u8], &[u8]) {
    let mut idx = 1;
    for i in input.iter() {
        if *i == c {
            break;
        }
        idx += 1;
    }

    (&input[idx..], &input[0..idx])
}
