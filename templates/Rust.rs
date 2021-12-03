#![feature(stdin_forwarders)]
use std::env;
use std::io;

fn main() {
    let nums = io::stdin()
        .lines()
        .map(|s| s.unwrap().parse::<u32>().unwrap());

    if env::args().nth(1).unwrap() == "1" {
        println!("{}", nums.sum::<u32>());
    } else {
        println!("{}", nums.product::<u32>());
    }
}
