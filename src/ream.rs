//! Parse hoon source into AST twig

use std::str;
use std::str::FromStr;

use twig::{Twig, Rune, Odor};

/// Parse Hoon source code into a Twig AST.
///
/// Return trailing input after a successful parse along with the resulting
/// Twig. Return trailing input after the point of failed parse on error.
pub fn ream(input: &[u8]) -> Result<(&[u8], Twig), &[u8]> {
    // TODO
    Err(input)
}
