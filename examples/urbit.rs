extern crate nock;
extern crate urbit;
extern crate num;

use std::env;
use std::io::prelude::*;
use std::fs::File;
use num::bigint::BigUint;
use num::traits::One;
use nock::{Noun, Shape};
use urbit::VM;

fn main() {
    let mut file = File::open(env::args()
                                  .nth(1)
                                  .expect("Usage urbit [urbit.pill]"))
                       .unwrap();

    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();
    println!("Unpacking pill");
    let noun = urbit::unpack_pill(buf).unwrap();
    println!("Unpacked pill");

    let count: BigUint = noun.fold(|x| {
        match x {
            Shape::Cell(p, q) => p + q,
            _ => One::one(),
        }
    });

    println!("Pill has {} atoms", count);

    println!("Nocking pill");
    let mut vm = VM::new();
    let noun = vm.nock_on(Noun::from(0u32), noun).unwrap();
    println!("Result: {}", noun);
}
