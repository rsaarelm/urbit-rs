//! Parse hoon source into AST twig

use std::str;
use std::str::FromStr;
use num::bigint::BigUint;
use num::traits::Zero;
use nock::Noun;

use twig::{self, Twig, Rune, Odor};

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
    twig(input)
}

fn twig(input: &[u8]) -> ParseResult<Twig> {
    use twig::Rune::*;

    if let Ok(x) = atom(input) {
        return Ok(x);
    }
    if let Ok(x) = wing(input) {
        return Ok(x);
    }

    // Just crunch through the whole set of rune data and look for all the
    // ones that look like they can be parsed naively.
    for i in twig::RUNES.iter() {
        if i.rune.is_regular() && i.rune.glyph().is_some() {
            if let Ok(x) = rune(input, i.rune) {
                return Ok(x);
            }
        }
    }

    Err(input)
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Form {
    Tall,
    Wide,
}

fn rune<'a>(input: &'a [u8], rune: Rune) -> ParseResult<'a, Twig> {
    assert!(rune.is_regular(),
            "Can't use standard parser on this type of rune!");
    let glyph = rune.glyph().expect("Trying to parse an unprintable rune");
    let (input, form) = try!(rune_start(input, &glyph));
    let (input, args) = try!(rune_args(input, form, rune.arity()));
    Ok((input, Twig::Cell(box Twig::Rune(rune), box args)))
}

/// Parse the start of a rune with the given glyph and determine if the rune
/// is in wide form (.*(p q) or tall form (.*  p  q).
fn rune_start<'a>(input: &'a [u8], glyph: &str) -> ParseResult<'a, Form> {
    assert!(glyph.len() == 2, "Rune isn't 2 characters");
    let (mut tail, _) = try!(tag(input, glyph));

    if let Ok((tail, _)) = tag(tail, "(") {
        return Ok((tail, Form::Wide));
    }

    if let Ok((tail, _)) = gap(tail) {
        return Ok((tail, Form::Tall));
    }

    Err(input)
}

fn rune_args(input: &[u8], form: Form, n: usize) -> ParseResult<Twig> {
    assert!(n > 0);
    if n == 1 {
        let (input, p) = try!(twig(input));
        let (input, _) = try!(rune_end(input, form));
        Ok((input, p))
    } else {
        let (input, p) = try!(twig(input));
        let (input, _) = try!(rune_sep(input, form));
        let (input, ps) = try!(rune_args(input, form, n - 1));
        Ok((input, Twig::Cell(box p, box ps)))
    }
}

fn rune_sep(input: &[u8], form: Form) -> ParseResult<&[u8]> {
    match form {
        Form::Tall => gap(input),
        Form::Wide => tag(input, " "),
    }
}

fn rune_end(input: &[u8], form: Form) -> ParseResult<&[u8]> {
    match form {
        Form::Tall => Ok((input, &input[..0])),
        Form::Wide => tag(input, ")"),
    }
}

fn atom(input: &[u8]) -> ParseResult<Twig> {
    if let Ok((tail, num)) = ud(input) {
        Ok((tail,
            Twig::Cell(box Twig::Rune(Rune::dtzy), box Twig::Atom(Odor::ud, num))))
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
        } else if idx > 0 && *c == '.' as u8 {
            // Ignore separator dots.
            // We're lazy, so we let them show up in any pattern after the
            // initial digit instead of insisting that they group the digits
            // in sized groups.
        } else {
            if idx == 0 {
                return Err(input);
            }
            break;
        }
        idx += 1;
    }

    Ok((&input[idx..], FromStr::from_str(&num).unwrap()))
}

fn wing(input: &[u8]) -> ParseResult<Twig> {
    let mut items = Vec::new();
    let mut tail = input;
    loop {
        if let Ok((t, term)) = term(tail) {
            let item = match str::from_utf8(term).unwrap() {
                // Dot is "subject", all of the preceding context. Instead of
                // an atom, we will insert the Nock formula for getting the
                // entire subject.
                "." => n![0, 1],
                // Buc is the default recursion symbol. Evaluates to atom
                // zero.
                "$" => Noun::Atom(0),
                // The regular terms are replaced with cord atoms.
                _ => Noun::from_bytes(term),
            };
            items.push(item);
            tail = t;
        }
        match wing_dot(tail) {
            Ok((t, _)) => {
                tail = t;
            }
            Err(_) => {
                if items.len() == 0 {
                    return Err(input);
                } else {
                    break;
                }
            }
        }
    }
    assert!(items.len() > 0);
    if items[0] == n![0, 1] {
        // Wings that start with dot produce a different twig, based on
        // experimenting with the real ream. Don't understand the full logic
        // of this yet.
        Ok((tail,
            Twig::Cell(box Twig::Rune(Rune::cnts),
                       box Twig::Cell(box Twig::Wing(items),
                                      // Empty tram. XXX: Maybe we get a better twig type for trams?
                                      // Should use that here then.
                                      box Twig::Atom(Odor::ud, Zero::zero())))))
    } else {
        Ok((tail,
            Twig::Cell(box Twig::Rune(Rune::cnzz), box Twig::Wing(items))))
    }
}

fn term(input: &[u8]) -> ParseResult<&[u8]> {
    // Special terms.
    for special in ["$", "."].into_iter() {
        if let Ok(x) = word(input, special) {
            return Ok(x);
        }
    }

    let mut idx = 0;
    for i in input.iter() {
        if idx == 0 && !is_ident_start(*i) {
            break;
        }

        if idx > 0 && !is_ident_middle(*i) {
            break;
        }

        idx += 1;
    }

    if idx == 0 {
        Err(input)
    } else {
        Ok((&input[idx..], &input[..idx]))
    }
}

fn wing_dot(input: &[u8]) -> ParseResult<()> {
    if input.len() < 2 {
        return Err(input);
    }
    let (tail, _) = try!(tag(input, "."));
    if !is_ident_start(tail[0]) && tail[0] != '.' as u8 {
        Err(input)
    } else {
        Ok((tail, ()))
    }
}

fn space_or_comment(input: &[u8]) -> ParseResult<()> {
    let mut tail = input;
    loop {
        if let Ok((t, _)) = comment(tail) {
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

/// At least one newline or at least two spaces.
///
/// Used to separate tall forms in Hoon.
fn gap(input: &[u8]) -> ParseResult<&[u8]> {
    let mut spaces = 0;
    let mut idx = 0;
    for i in input.iter() {
        if *i == '\n' as u8 {
            spaces += 2;
        } else if is_whitespace(*i) {
            spaces += 1;
        } else {
            break;
        }

        idx += 1;
    }

    if spaces < 2 {
        Err(input)
    } else {
        Ok((&input[idx..], &input[0..idx]))
    }
}

fn comment(input: &[u8]) -> ParseResult<&[u8]> {
    let (input, _) = try!(tag(input, "::"));
    // XXX: The initial bit with the "::" won't make it to the return value.
    split_after(input, '\n' as u8)
}

fn is_whitespace(c: u8) -> bool {
    // Physical tabs not included. A physical tab in the code is a syntax
    // error.
    c == 0x10 || c == 0x13 || c == ' ' as u8
}

fn is_lowercase(c: u8) -> bool {
    c >= 'a' as u8 && c <= 'z' as u8
}

fn is_numeric(c: u8) -> bool {
    c >= '0' as u8 && c <= '9' as u8
}

fn is_ident_start(c: u8) -> bool {
    is_lowercase(c)
}

fn is_ident_middle(c: u8) -> bool {
    is_lowercase(c) || is_numeric(c) || c == '-' as u8
}

fn tag<'a>(input: &'a [u8], prefix: &str) -> ParseResult<'a, &'a [u8]> {
    let prefix = prefix.as_bytes();
    if input.len() < prefix.len() {
        return Err(input);
    }

    for (x, y) in prefix.iter().zip(input.iter()) {
        if x != y {
            return Err(input);
        }
    }

    Ok((&input[prefix.len()..], &input[0..prefix.len()]))
}

/// Parse a full word.
fn word<'a>(input: &'a [u8], s: &str) -> ParseResult<'a, &'a [u8]> {
    let ret = try!(tag(input, s));
    try!(word_break(input));
    Ok(ret)
}

fn word_break(input: &[u8]) -> ParseResult<()> {
    if input.len() == 0 {
        return Ok((input, ()));
    }
    if is_ident_middle(input[0]) {
        return Err(input);
    }
    Ok((input, ()))
}

fn split_after(input: &[u8], c: u8) -> ParseResult<&[u8]> {
    let mut idx = 1;
    for i in input.iter() {
        if *i == c {
            break;
        }
        idx += 1;
    }
    Ok((&input[idx..], &input[0..idx]))
}

#[cfg(test)]
mod test {
    use std::fmt;
    use std::str;
    use super::{ParseResult, ream};

    fn parses<T: fmt::Debug>(ret: ParseResult<T>, expect: &str) {
        assert_eq!(&format!("{:?}", ret.unwrap().1), expect);
    }

    #[test]
    fn test_parse() {
       parses(ream("123".as_bytes()), "Cell(Rune(dtzy), Atom(ud, BigUint { data: [123] }))");
       parses(ream("=>($ 3)".as_bytes()), "Cell(Rune(tsgr), Cell(Cell(Rune(cnzz), Wing([\"$\"])), Cell(Rune(dtzy), Atom(ud, BigUint { data: [3] }))))");
    }
}
