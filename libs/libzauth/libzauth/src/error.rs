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

use rustc_serialize::base64::FromBase64Error;
use std::error;
use std::fmt;
use std::io;

#[derive(Debug)]
pub enum Error {
    Base64,
    Expired,
    Invalid(&'static str),
    Io(io::Error),
    Missing(&'static str),
    Parse,
    SignatureMismatch,
    UnknownKey(usize)
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Error::Base64            => write!(f, "error decoding base64"),
            Error::Expired           => write!(f, "expired"),
            Error::Invalid(ref s)    => write!(f, "invalid: \"{}\"", s),
            Error::Io(ref e)         => write!(f, "i/o: {}", e),
            Error::Missing(ref s)    => write!(f, "missing: \"{}\"", s),
            Error::Parse             => write!(f, "parse error"),
            Error::SignatureMismatch => write!(f, "signature does not match"),
            Error::UnknownKey(i)     => write!(f, "unknown key {}", i),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        "zauth error"
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Io(ref e) => Some(e),
            _                => None
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Io(e)
    }
}

impl From<FromBase64Error> for Error {
    fn from(_: FromBase64Error) -> Error {
        Error::Base64
    }
}

