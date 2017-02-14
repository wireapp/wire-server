extern crate clap;
extern crate zauth;

use clap::{Arg, App};
use std::path::Path;
use std::process;
use zauth::{Token, Keystore};

pub fn main() {
    let args = App::new("verify")
        .about("Test application verifying tokens")
        .arg(Arg::with_name("store")
             .required(true)
             .short("s")
             .long("store")
             .value_name("FILE")
             .help("Keystore")
             .takes_value(true))
        .arg(Arg::with_name("token")
             .required(true)
             .short("t")
             .long("token")
             .value_name("STRING")
             .help("Token string to verify")
             .takes_value(true))
        .get_matches();

    let path   = args.value_of("store").unwrap();
    let string = args.value_of("token").unwrap();

    let ks = Keystore::open(&Path::new(&path)).unwrap_or_else(|e| {
        println!("Failed to open keystore: {}", e);
        process::exit(1)
    });

    let tk = Token::parse(&string).unwrap_or_else(|e| {
        println!("Failed to parse token string: {}", e);
        process::exit(2)
    });

    tk.verify(&ks).unwrap_or_else(|e| {
        println!("Failed to verify token: {}", e);
        process::exit(3)
    })
}
