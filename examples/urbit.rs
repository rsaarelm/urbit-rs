extern crate nock;
extern crate urbit;

use std::env;
use std::rc::Rc;
use std::io::prelude::*;
use std::fs::File;
use nock::Noun::*;

fn main() {
    let mut file = File::open(env::args().nth(1).expect("Usage scan [urbit.pill]")).unwrap();

    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();
    println!("Unpacking pill");
    let noun = urbit::unpack_pill(buf).unwrap();
    let noun = Cell(Rc::new(Atom(0)), Rc::new(noun));

    println!("Nocking pill");
    let noun = noun.nock().unwrap();
    println!("Result: {}", noun);
}
