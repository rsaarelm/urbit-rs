extern crate time;
extern crate nock;
extern crate urbit;

use std::env;
use std::rc::Rc;
use std::io::prelude::*;
use std::fs::File;
use nock::Noun::*;
use nock::nock_on;

fn main() {
    let mut file = File::open(env::args().nth(1).expect("Usage scan [urbit.pill]")).unwrap();

    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();
    println!("Unpacking pill");
    let time_start = time::precise_time_s();
    let noun = urbit::unpack_pill(buf).unwrap();
    println!("Unpacked pill in {:.03} seconds.", time::precise_time_s() - time_start);

    println!("Nocking pill");
    let noun = nock_on(&Atom(0), &noun).unwrap();
    println!("Result: {}", noun);
}
