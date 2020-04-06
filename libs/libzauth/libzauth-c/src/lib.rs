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

extern crate libc;
extern crate zauth;

use libc::{size_t, uint8_t};
use std::char;
use std::fs::File;
use std::io::{self, BufReader, Read};
use std::path::Path;
use std::ptr;
use std::slice;
use std::str;
use std::panic::{self, UnwindSafe};
use zauth::{Acl, Error, Keystore, Token, TokenType};
use zauth::acl;

/// Variant of std::try! that returns the unwrapped error.
macro_rules! try_unwrap {
    ($expr:expr) => {
        match $expr {
            Ok(x)  => x,
            Err(e) => return From::from(e)
        }
    }
}

#[repr(C)]
#[no_mangle]
#[derive(Clone, Copy, Debug)]
pub struct Range {
    ptr: *const u8,
    len: size_t
}

pub struct ZauthAcl(zauth::Acl);
pub struct ZauthKeystore(zauth::Keystore);
pub struct ZauthToken(zauth::Token<'static>);

#[no_mangle]
pub extern fn zauth_keystore_open(f: *const uint8_t, n: size_t, s: *mut *mut ZauthKeystore) -> ZauthResult {
    if f.is_null() {
        return ZauthResult::NullArg;
    }
    catch_unwind(|| {
        let bytes = unsafe { slice::from_raw_parts(f, n) };
        let path  = try_unwrap!(str::from_utf8(bytes));
        let store = try_unwrap!(Keystore::open(&Path::new(path)));
        unsafe {
            *s= Box::into_raw(Box::new(ZauthKeystore(store)));
        }
        ZauthResult::Ok
    })
}

#[no_mangle]
pub extern fn zauth_keystore_delete(s: *mut ZauthKeystore) {
    catch_unwind(|| {
        unsafe { Box::from_raw(s); }
        ZauthResult::Ok
    });
}

#[no_mangle]
pub extern fn zauth_acl_open(f: *const uint8_t, n: size_t, a: *mut *mut ZauthAcl) -> ZauthResult {
    if f.is_null() {
        return ZauthResult::NullArg;
    }
    catch_unwind(|| {
        let bytes = unsafe { slice::from_raw_parts(f, n) };
        let path  = try_unwrap!(str::from_utf8(bytes));
        let mut rdr = BufReader::new(try_unwrap!(File::open(&Path::new(path))));
        let mut txt = String::new();
        try_unwrap!(rdr.read_to_string(&mut txt));
        let acl = try_unwrap!(Acl::from_str(&txt));
        unsafe {
            *a= Box::into_raw(Box::new(ZauthAcl(acl)));
        }
        ZauthResult::Ok
    })
}

#[no_mangle]
pub extern fn zauth_acl_delete(a: *mut ZauthAcl) {
    catch_unwind(|| {
        unsafe { Box::from_raw(a); }
        ZauthResult::Ok
    });
}

#[no_mangle]
pub extern fn zauth_token_parse(cs: *const uint8_t, n: size_t, zt: *mut *mut ZauthToken) -> ZauthResult {
    if cs.is_null() {
        return ZauthResult::NullArg;
    }
    catch_unwind(|| {
        let b = unsafe { slice::from_raw_parts(cs, n) };
        let s = try_unwrap!(str::from_utf8(b));
        let t = try_unwrap!(Token::parse(s));
        unsafe {
            *zt = Box::into_raw(Box::new(ZauthToken(t)));
        }
        ZauthResult::Ok
    })
}

#[no_mangle]
pub extern fn zauth_token_verify(t: &ZauthToken, s: &ZauthKeystore) -> ZauthResult {
    catch_unwind(|| {
        try_unwrap!(t.0.verify(&s.0));
        ZauthResult::Ok
    })
}

#[no_mangle]
pub extern
fn zauth_token_allowed(t: &ZauthToken, acl: &ZauthAcl, cp: *const uint8_t, n: size_t, out: *mut uint8_t) -> ZauthResult {
    catch_unwind(|| {
        let b = unsafe { slice::from_raw_parts(cp, n) };
        let s = try_unwrap!(str::from_utf8(b));
        if t.0.has_access(&acl.0, s) {
            unsafe { *out = 1 }
        } else {
            unsafe { *out = 0 }
        }
        ZauthResult::Ok
    })
}

#[no_mangle]
pub extern fn zauth_token_type(t: &ZauthToken) -> ZauthTokenType {
    From::from(t.0.token_type)
}

// Commented out, looks unused, and causing portability issues with ia32.
//#[no_mangle]
//pub extern fn zauth_token_time(t: &ZauthToken) -> c_long {
//    t.0.timestamp
//}

#[no_mangle]
pub extern fn zauth_token_version(t: &ZauthToken) -> uint8_t {
    t.0.version
}

#[no_mangle]
pub extern fn zauth_token_lookup(t: &ZauthToken, c: uint8_t) -> Range {
    if let Some(k) = char::from_u32(c as u32) {
        if let Some(s) = t.0.lookup(k) {
            return Range { ptr: s.as_ptr(), len: s.len() }
        }
    }
    Range { ptr: ptr::null(), len: 0 }
}

#[no_mangle]
pub extern fn zauth_token_delete(t: *mut ZauthToken) {
    catch_unwind(|| {
        unsafe { Box::from_raw(t); }
        ZauthResult::Ok
    });
}

#[repr(C)]
#[no_mangle]
#[derive(Clone, Copy, Debug)]
pub enum ZauthTokenType {
    User            = 0,
    Bot             = 1,
    Access          = 2,
    Provider        = 4,
    LegalHoldUser   = 5,
    LegalHoldAccess = 6,
    Unknown         = 3
}

impl From<TokenType> for ZauthTokenType {
    fn from(t: TokenType) -> ZauthTokenType {
        match t {
            TokenType::User              => ZauthTokenType::User,
            TokenType::Access            => ZauthTokenType::Access,
            TokenType::Bot               => ZauthTokenType::Bot,
            TokenType::Provider          => ZauthTokenType::Provider,
            TokenType::LegalHoldUser     => ZauthTokenType::LegalHoldUser,
            TokenType::LegalHoldAccess   => ZauthTokenType::LegalHoldAccess,
            TokenType::Unknown           => ZauthTokenType::Unknown
        }
    }
}

#[repr(C)]
#[no_mangle]
#[derive(Clone, Copy, Debug)]
pub enum ZauthResult {
    Ok                = 0,
    Base64Error       = 1,
    Expired           = 2,
    InvalidAttr       = 3,
    IoError           = 4,
    MissingAttr       = 5,
    NullArg           = 6,
    ParseError        = 7,
    SignatureMismatch = 8,
    UnknownKey        = 9,
    Utf8Error         = 10,
    AclError          = 11,
    Panic             = 99
}

impl From<zauth::Error> for ZauthResult {
    fn from(e: zauth::Error) -> ZauthResult {
        match e {
            Error::Base64            => ZauthResult::Base64Error,
            Error::Expired           => ZauthResult::Expired,
            Error::Invalid(_)        => ZauthResult::InvalidAttr,
            Error::Io(_)             => ZauthResult::IoError,
            Error::Missing(_)        => ZauthResult::MissingAttr,
            Error::Parse             => ZauthResult::ParseError,
            Error::SignatureMismatch => ZauthResult::SignatureMismatch,
            Error::UnknownKey(_)     => ZauthResult::UnknownKey,
        }
    }
}

impl From<acl::Error> for ZauthResult {
    fn from(_: acl::Error) -> ZauthResult {
        ZauthResult::AclError
    }
}

impl From<str::Utf8Error> for ZauthResult {
    fn from(_: str::Utf8Error) -> ZauthResult {
        ZauthResult::Utf8Error
    }
}

impl From<io::Error> for ZauthResult {
    fn from(_: io::Error) -> ZauthResult {
        ZauthResult::IoError
    }
}

fn catch_unwind<F>(f: F) -> ZauthResult
  where F: FnOnce() -> ZauthResult + UnwindSafe {
    match panic::catch_unwind(f) {
        Ok(x)  => x,
        Err(_) => ZauthResult::Panic
    }
}
