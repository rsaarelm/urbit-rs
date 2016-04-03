extern crate nock;
extern crate urbit;
extern crate num;

use std::env;
use std::io::prelude::*;
use std::fs::File;
use num::bigint::BigUint;
use num::traits::One;
use nock::{Nock, Noun, Shape};
use urbit::VM;

fn get_size(noun: &Noun) -> BigUint {
    noun.fold(|x| {
        match x {
            Shape::Cell(p, q) => p + q,
            _ => One::one(),
        }
    })
}

fn main() {
    let mut file = File::open(env::args()
                                  .nth(1)
                                  .expect("Usage urbit [urbit.pill]"))
                       .unwrap();

    let mut buf = Vec::new();
    file.read_to_end(&mut buf).unwrap();
    println!("Unpacking pill");
    let pill = urbit::unpack_pill(buf).unwrap();

    // You can't nock the whole pill as an
    let (kernel, arvo) = match pill.get() {
        Shape::Cell(kernel, arvo) => ((*kernel).clone(), (*arvo).clone()),
        _ => panic!("bad"),
    };
    println!("Unpacked pill");

    println!("Kernel has {} atoms", get_size(&kernel));
    println!("Arvo has {} atoms", get_size(&arvo));

    let mut vm = VM::new();

    println!("Nocking kernel");
    let noun = vm.nock_on(Noun::from(0u32), kernel).unwrap();
    vm.print_status();
    println!("Kernel nocked, subject has {} atoms", get_size(&noun));
    println!("{}", noun);

    // FIXME: This isn't the right way to boot...
    println!("Nocking arvo");
    let noun = vm.nock_on(Noun::from(0u32), arvo).unwrap();
    vm.print_status();
    println!("Arvo nocked, subject has {} atoms", get_size(&noun));
    println!("{}", noun);
}
