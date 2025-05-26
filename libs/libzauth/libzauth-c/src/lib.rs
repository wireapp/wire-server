// This file is part of the Wire Server implementation.
//
// Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

use libc::size_t;
use std::char;
use std::ffi::{CString, NulError};
use std::fs::File;
use std::io::{self, BufReader, Read};
use std::panic::{self, UnwindSafe};
use std::path::Path;
use std::ptr;
use std::slice;
use std::str;
use zauth::acl;
use zauth::{
    verify_oauth_token, Acl, Error, Keystore, OauthError, Token, TokenType, TokenVerification,
};

/// Variant of std::try! that returns the unwrapped error.
macro_rules! try_unwrap {
    ($expr:expr) => {
        match $expr {
            Ok(x) => x,
            Err(e) => return From::from(e),
        }
    };
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct Range {
    ptr: *const u8,
    len: size_t,
}

pub struct ZauthAcl(zauth::Acl);
pub struct ZauthKeystore(zauth::Keystore);
pub struct ZauthToken(zauth::Token<'static>);

#[no_mangle]
pub extern "C" fn zauth_keystore_open(
    f: *const u8,
    n: size_t,
    s: *mut *mut ZauthKeystore,
) -> ZauthResult {
    if f.is_null() {
        return ZauthResult::NullArg;
    }
    catch_unwind(|| {
        let bytes = unsafe { slice::from_raw_parts(f, n) };
        let path = try_unwrap!(str::from_utf8(bytes));
        let store = try_unwrap!(Keystore::open(&Path::new(path)));
        unsafe {
            *s = Box::into_raw(Box::new(ZauthKeystore(store)));
        }
        ZauthResult::Ok
    })
}

#[no_mangle]
pub extern "C" fn zauth_keystore_delete(s: *mut ZauthKeystore) {
    catch_unwind(|| {
        unsafe {
            drop(Box::from_raw(s));
        }
        ZauthResult::Ok
    });
}

#[no_mangle]
pub extern "C" fn zauth_acl_open(f: *const u8, n: size_t, a: *mut *mut ZauthAcl) -> ZauthResult {
    if f.is_null() {
        return ZauthResult::NullArg;
    }
    catch_unwind(|| {
        let bytes = unsafe { slice::from_raw_parts(f, n) };
        let path = try_unwrap!(str::from_utf8(bytes));
        let mut rdr = BufReader::new(try_unwrap!(File::open(&Path::new(path))));
        let mut txt = String::new();
        try_unwrap!(rdr.read_to_string(&mut txt));
        let acl = try_unwrap!(Acl::from_str(&txt));
        unsafe {
            *a = Box::into_raw(Box::new(ZauthAcl(acl)));
        }
        ZauthResult::Ok
    })
}

#[no_mangle]
pub extern "C" fn zauth_acl_delete(a: *mut ZauthAcl) {
    catch_unwind(|| {
        unsafe {
            drop(Box::from_raw(a));
        }
        ZauthResult::Ok
    });
}

#[no_mangle]
pub extern "C" fn zauth_token_parse(
    cs: *const u8,
    n: size_t,
    zt: *mut *mut ZauthToken,
) -> ZauthResult {
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
pub extern "C" fn zauth_token_verify(t: &mut ZauthToken, s: &ZauthKeystore) -> ZauthResult {
    let result = catch_unwind(|| {
        try_unwrap!(t.0.verify(&s.0));
        ZauthResult::Ok
    });
    match result {
        ZauthResult::Ok => t.0.verification = TokenVerification::Verified,
        _ => t.0.verification = TokenVerification::Invalid,
    };
    result
}

#[no_mangle]
pub extern "C" fn zauth_token_allowed(
    t: &ZauthToken,
    acl: &ZauthAcl,
    cp: *const u8,
    n: size_t,
    out: *mut u8,
) -> ZauthResult {
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
pub extern "C" fn zauth_token_type(t: &ZauthToken) -> ZauthTokenType {
    From::from(t.0.token_type)
}

#[no_mangle]
pub extern "C" fn zauth_token_verification(t: &ZauthToken) -> ZauthTokenVerification {
    From::from(t.0.verification)
}

#[no_mangle]
pub extern fn zauth_token_time(t: &ZauthToken) -> i64 {
    t.0.timestamp
}

#[no_mangle]
pub extern "C" fn zauth_token_version(t: &ZauthToken) -> u8 {
    t.0.version
}

#[no_mangle]
pub extern "C" fn zauth_token_lookup(t: &ZauthToken, c: u8) -> Range {
    if let Some(k) = char::from_u32(c as u32) {
        if let Some(s) = t.0.lookup(k) {
            return Range {
                ptr: s.as_ptr(),
                len: s.len(),
            };
        }
    }
    Range {
        ptr: ptr::null(),
        len: 0,
    }
}

#[no_mangle]
pub extern "C" fn zauth_token_delete(t: *mut ZauthToken) {
    catch_unwind(|| {
        unsafe {
            drop(Box::from_raw(t));
        }
        ZauthResult::Ok
    });
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub enum ZauthTokenType {
    User = 0,
    Bot = 1,
    Access = 2,
    Provider = 4,
    LegalHoldUser = 5,
    LegalHoldAccess = 6,
    Unknown = 3,
}

impl From<TokenType> for ZauthTokenType {
    fn from(t: TokenType) -> ZauthTokenType {
        match t {
            TokenType::User => ZauthTokenType::User,
            TokenType::Access => ZauthTokenType::Access,
            TokenType::Bot => ZauthTokenType::Bot,
            TokenType::Provider => ZauthTokenType::Provider,
            TokenType::LegalHoldUser => ZauthTokenType::LegalHoldUser,
            TokenType::LegalHoldAccess => ZauthTokenType::LegalHoldAccess,
            TokenType::Unknown => ZauthTokenType::Unknown,
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub enum ZauthResult {
    Ok = 0,
    Base64Error = 1,
    Expired = 2,
    InvalidAttr = 3,
    IoError = 4,
    MissingAttr = 5,
    NullArg = 6,
    ParseError = 7,
    SignatureMismatch = 8,
    UnknownKey = 9,
    Utf8Error = 10,
    AclError = 11,
    Panic = 99,
}

impl From<zauth::Error> for ZauthResult {
    fn from(e: zauth::Error) -> ZauthResult {
        match e {
            Error::Base64 => ZauthResult::Base64Error,
            Error::Expired => ZauthResult::Expired,
            Error::Invalid(_) => ZauthResult::InvalidAttr,
            Error::Io(_) => ZauthResult::IoError,
            Error::Missing(_) => ZauthResult::MissingAttr,
            Error::Parse => ZauthResult::ParseError,
            Error::SignatureMismatch => ZauthResult::SignatureMismatch,
            Error::UnknownKey(_) => ZauthResult::UnknownKey,
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
where
    F: FnOnce() -> ZauthResult + UnwindSafe,
{
    match panic::catch_unwind(f) {
        Ok(x) => x,
        Err(_) => ZauthResult::Panic,
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub enum ZauthTokenVerification {
    Verified = 0,
    Invalid = 1,
    Pending = 2,
}

impl From<TokenVerification> for ZauthTokenVerification {
    fn from(t: TokenVerification) -> ZauthTokenVerification {
        match t {
            TokenVerification::Verified => ZauthTokenVerification::Verified,
            TokenVerification::Invalid => ZauthTokenVerification::Invalid,
            TokenVerification::Pending => ZauthTokenVerification::Pending,
        }
    }
}

// ------------------------------------------------------------------------
// OAuth

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub enum OAuthResultStatus {
    Ok = 0,
    InsufficientScope = 1,
    NullArg = 2,
    IoError = 3,
    Utf8Error = 4,
    Panic = 99,
}

pub struct OAuthPubJwk(String);

#[repr(C)]
#[derive(Clone, Debug)]
pub struct OAuthResult {
    uid: *const libc::c_char,
    status: OAuthResultStatus,
}

fn catch_unwind_with<F, R>(f: F, r: R) -> R
where
    F: FnOnce() -> R + UnwindSafe,
{
    match panic::catch_unwind(f) {
        Ok(x) => x,
        Err(_) => r,
    }
}

impl From<OauthError> for OAuthResultStatus {
    fn from(e: OauthError) -> Self {
        match e {
            OauthError::JsonError(_) => Self::Panic,
            OauthError::JwtSimpleError(_) => Self::Panic,
            OauthError::Base64DecodeError(_) => Self::Panic,
            OauthError::InvalidJwk => Self::Panic,
            OauthError::InvalidJwtNoSubject => Self::Panic,
            OauthError::InvalidScope => Self::InsufficientScope,
        }
    }
}

impl From<io::Error> for OAuthResultStatus {
    fn from(_: io::Error) -> Self {
        Self::IoError
    }
}

impl From<str::Utf8Error> for OAuthResultStatus {
    fn from(_: str::Utf8Error) -> Self {
        Self::Utf8Error
    }
}

impl From<str::Utf8Error> for OAuthResult {
    fn from(_: str::Utf8Error) -> Self {
        OAuthResult {
            uid: ptr::null(),
            status: OAuthResultStatus::Utf8Error,
        }
    }
}

impl From<NulError> for OAuthResult {
    fn from(_: NulError) -> Self {
        OAuthResult {
            uid: ptr::null(),
            status: OAuthResultStatus::Panic,
        }
    }
}

impl From<OauthError> for OAuthResult {
    fn from(e: OauthError) -> Self {
        OAuthResult {
            uid: ptr::null(),
            status: e.into(),
        }
    }
}

#[no_mangle]
pub extern "C" fn oauth_key_open(
    f: *const u8,
    n: size_t,
    k: *mut *mut OAuthPubJwk,
) -> OAuthResultStatus {
    if f.is_null() {
        return OAuthResultStatus::NullArg;
    }
    catch_unwind_with(
        || {
            let bytes = unsafe { slice::from_raw_parts(f, n) };
            let path = try_unwrap!(str::from_utf8(bytes));
            let mut rdr = BufReader::new(try_unwrap!(File::open(&Path::new(path))));
            let mut txt = String::new();
            try_unwrap!(rdr.read_to_string(&mut txt));
            unsafe {
                *k = Box::into_raw(Box::new(OAuthPubJwk(txt)));
            }
            OAuthResultStatus::Ok
        },
        OAuthResultStatus::Panic,
    )
}

#[no_mangle]
pub extern "C" fn oauth_key_delete(a: *mut OAuthPubJwk) {
    catch_unwind_with(
        || {
            unsafe {
                drop(Box::from_raw(a));
            }
            OAuthResultStatus::Ok
        },
        OAuthResultStatus::Panic,
    );
}

#[no_mangle]
pub extern "C" fn oauth_verify_token(
    jwk: &OAuthPubJwk,
    token: *const u8,
    token_len: size_t,
    scope: *const u8,
    scope_len: size_t,
    method: *const u8,
    method_len: size_t,
) -> OAuthResult {
    match panic::catch_unwind(|| {
        if token.is_null() {
            return OAuthResult {
                uid: ptr::null(),
                status: OAuthResultStatus::NullArg,
            };
        }
        if scope.is_null() {
            return OAuthResult {
                uid: ptr::null(),
                status: OAuthResultStatus::NullArg,
            };
        }
        if method.is_null() {
            return OAuthResult {
                uid: ptr::null(),
                status: OAuthResultStatus::NullArg,
            };
        }
        let bytes = unsafe { slice::from_raw_parts(token, token_len) };
        let token = try_unwrap!(str::from_utf8(bytes));
        let bytes = unsafe { slice::from_raw_parts(scope, scope_len) };
        let scope = try_unwrap!(str::from_utf8(bytes));
        let bytes = unsafe { slice::from_raw_parts(method, method_len) };
        let method = str::from_utf8(bytes).unwrap();
        let subject = try_unwrap!(verify_oauth_token(&jwk.0, token, scope, method));
        let c_str = try_unwrap!(CString::new(subject));
        OAuthResult {
            uid: c_str.into_raw(),
            status: OAuthResultStatus::Ok,
        }
    }) {
        Ok(x) => x,
        Err(_) => OAuthResult {
            uid: ptr::null(),
            status: OAuthResultStatus::Panic,
        },
    }
}

#[no_mangle]
pub extern "C" fn oauth_result_uid_delete(s: *mut libc::c_char) {
    catch_unwind_with(
        || {
            unsafe {
                if s.is_null() {
                    return OAuthResultStatus::Ok;
                }
                CString::from_raw(s)
            };
            OAuthResultStatus::Ok
        },
        OAuthResultStatus::Panic,
    );
}
