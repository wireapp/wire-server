// This file is part of the Wire Server implementation.
//
// Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
//
// This program is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License along
// with this program. If not, see <https://www.gnu.org/licenses/>.

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
