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

use rustc_serialize::base64::FromBase64;
use sodiumoxide::crypto::sign::ed25519::{PublicKey, Signature, verify_detached};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::str::{self, FromStr};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use acl::Acl;
use error::Error;

// Keystore /////////////////////////////////////////////////////////////////

pub struct Keystore(Vec<PublicKey>);

impl Keystore {
    pub fn new() -> Keystore {
        Keystore(Vec::new())
    }

    pub fn open(p: &Path) -> Result<Keystore, Error> {
        let reader = BufReader::new(try!(File::open(p)));
        let mut keys = Vec::new();
        for line in reader.lines() {
            let decoded = try!(try!(line).from_base64());
            match PublicKey::from_slice(&decoded) {
                None    => return Err(Error::Invalid("public key")),
                Some(k) => keys.push(k)
            }
        }
        Ok(Keystore(keys))
    }

    pub fn add(&mut self, key: PublicKey) {
        self.0.push(key)
    }

    pub fn key(&self, i: usize) -> Option<&PublicKey> {
        self.0.get(if i == 0 { 0 } else { i - 1 })
    }
}

// Token Type ////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    User,
    Access,
    Bot,
    Provider,
    LegalHoldUser,
    LegalHoldAccess,
    Unknown
}

impl FromStr for TokenType {
    type Err = ();
    fn from_str(s: &str) -> Result<TokenType, ()> {
        match s {
            "u"  => Ok(TokenType::User),
            "a"  => Ok(TokenType::Access),
            "b"  => Ok(TokenType::Bot),
            "p"  => Ok(TokenType::Provider),
            "lu" => Ok(TokenType::LegalHoldUser),
            "la" => Ok(TokenType::LegalHoldAccess),
            _    => Ok(TokenType::Unknown)
        }
    }
}

// Token ////////////////////////////////////////////////////////////////////

// Used when parsing tokens.
macro_rules! to_field {
    ($test: expr, $msg: expr) => {
        match $test {
            Some(x) =>
                match FromStr::from_str(x).ok() {
                    None    => return Err(Error::Invalid($msg)),
                    Some(v) => v
                },
            None => return Err(Error::Missing($msg))
        }
    }
}

#[derive(Debug)]
pub struct Token<'r> {
    pub signature:  Signature,
    pub version:    u8,
    pub key_idx:    usize,
    pub timestamp:  i64,
    pub token_type: TokenType,
    pub token_tag:  Option<&'r str>,
    meta:           HashMap<char, &'r str>,
    data:           &'r [u8]
}

impl<'r> Token<'r> {
    pub fn parse(s: &'r str) -> Result<Token<'r>, Error> {
        let (sgn, data) =
            match s.find('.') {
                Some(i) => s.split_at(i),
                None    => return Err(Error::Parse)
            };

        let signature =
            match Signature::from_slice(&try!(sgn.from_base64())) {
                Some(s) => s,
                None    => return Err(Error::Invalid("signature"))
            };

        let mut meta = HashMap::new();

        for p in data[1..].split('.') {
            let mut kv = p.split('=');
            let     ch = kv.next().and_then(|k| k.chars().next());
            let     vl = kv.next();
            match (ch, vl) {
                (Some(k), Some(v)) => { meta.insert(k, v); }
                _                  => return Err(Error::Parse)
            }
        }

        Ok(Token {
            signature:  signature,
            version:    to_field!(meta.remove(&'v'), "version"),
            key_idx:    to_field!(meta.remove(&'k'), "key index"),
            timestamp:  to_field!(meta.remove(&'d'), "timestamp"),
            token_type: to_field!(meta.remove(&'t'), "type"),
            token_tag:  meta.remove(&'l')
                            .and_then(|t| if t == "" { None } else { Some(t) }),
            meta:       meta,
            data:       data[1..].as_bytes()
        })
    }

    pub fn lookup(&self, k: char) -> Option<&str> {
        self.meta.get(&k).cloned()
    }

    pub fn verify(&self, store: &Keystore) -> Result<(), Error> {
        match self.token_type {
            TokenType::Access | TokenType::User | TokenType::Provider | TokenType::LegalHoldUser | TokenType::LegalHoldAccess =>
                if self.is_expired() {
                    Err(Error::Expired)
                } else {
                    self.signature_check(store)
                },
            TokenType::Bot =>
                if self.timestamp != -1 {
                    Err(Error::Invalid("timestamp"))
                } else {
                    self.signature_check(store)
                },
            TokenType::Unknown => Err(Error::Invalid("type"))
        }
    }

    pub fn has_access(&self, acl: &Acl, path: &str) -> bool {
        match self.token_type {
            TokenType::User             => acl.allowed("u", path),
            TokenType::Access           => acl.allowed("a", path),
            TokenType::Bot              => acl.allowed("b", path),
            TokenType::Provider         => acl.allowed("p", path),
            TokenType::LegalHoldUser    => acl.allowed("lu", path),
            TokenType::LegalHoldAccess  => acl.allowed("la", path),
            TokenType::Unknown          => false
        }
    }

    fn is_expired(&self) -> bool {
        let expiration = UNIX_EPOCH + Duration::from_secs(self.timestamp as u64);
        expiration < SystemTime::now()
    }

    fn signature_check(&self, store: &Keystore) -> Result<(), Error> {
        match store.key(self.key_idx) {
            Some(k) =>
                if verify_detached(&self.signature, self.data, k) {
                    Ok(())
                } else {
                    Err(Error::SignatureMismatch)
                },
            None => Err(Error::UnknownKey(self.key_idx))
        }
    }
}

// Tests ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use rustc_serialize::base64::FromBase64;
    use super::*;

    const ACCESS_TOKEN: &'static str =
        "aEPOxMwUriGEv2qc7Pb672ygy-6VeJ-8VrX3jmwalZr7xygU4izyCWxiT7IXfybnNGIsk1FQPb0RRVPx1s2UCw==.v=1.k=1.d=1466770783.t=a.l=.u=6562d941-4f40-4db4-b96e-56a06d71c2c3.c=11019722839397809329";

    const USER_TOKEN: &'static str =
        "vpJs7PEgwtsuzGlMY0-Vqs22s8o9ZDlp7wJrPmhCgIfg0NoTAxvxq5OtknabLMfNTEW9amn5tyeUM7tbFZABBA==.v=1.k=1.d=1466770905.t=u.l=.u=6562d941-4f40-4db4-b96e-56a06d71c2c3.r=4feacc";

    const BOT_TOKEN: &'static str =
        "-cEsTNb68hb-By81MZd5fF6NMDVzR_emkV_HfOnIdZTXsoeRRRZA7hmv9y2uLUNWDifNd-B8u0AjiAT_2rzUDg==.v=1.k=1.d=-1.t=b.l=.p=cd57deb3-bab6-46fd-be28-a3d48ef2c6b7.b=b46833f9-ec2a-4c4a-8304-1a367f849467.c=ae3d1b9e-e47c-4e10-a751-e99a64ada74b";

    const SESSION_TOKEN: &'static str =
        "hmTE4dWsW3TuOXtvAuSuHMcEHxT4MCqGrJ2hCw0YLZ1_XnjSc3ByohekeSrz7zjmEzHM-QSkg8MbrawR-kcjBQ==.v=1.k=1.d=1466771315.t=u.l=s.u=6562d941-4f40-4db4-b96e-56a06d71c2c3.r=4feacc";

    const PROVIDER_TOKEN: &'static str =
        "qcJ9zxFHMaiqj-tauhywI435BBs8t6wFyXAShkSQqaHK9r36k012rJYJIE7TTCHlFaGOzsk6E7h5G8JkLVjFDg==.v=1.k=1.d=1467640768.t=p.l=.p=84fa6cbf-0845-42cf-93b5-1e2195c68e11";

    const LEGAL_HOLD_ACCESS_TOKEN: &'static str =
        "6wca6kIO7_SFAev_Pl2uS6cBdkKuGk6MIh8WBK_ivZnwtRVrXF2pEHiocUWQZDy8YTrEweTJrqxUDptA7M1SBA==.v=1.k=1.d=1558361639.t=la.l=.u=4763099d-ab9b-4720-a1a3-558877f8b3e2.c=17967041325642812284";

    const LEGAL_HOLD_USER_TOKEN: &'static str =
        "GsydW1LQvwGYBGFErvqcqJvcipumtcdfVL4Li83KwR1ucnm-IrPM40SKl9Rhsdv0sqF_MF_eyTqMe_XpXR81Cg==.v=1.k=1.d=1558361914.t=lu.l=.u=ca754009-bc1e-4ef7-8384-dfec056bcc97.r=64";

    #[test]
    fn parse_access() {
        let t = Token::parse(ACCESS_TOKEN).unwrap();
        assert_eq!(t.signature.0[..], "aEPOxMwUriGEv2qc7Pb672ygy-6VeJ-8VrX3jmwalZr7xygU4izyCWxiT7IXfybnNGIsk1FQPb0RRVPx1s2UCw==".from_base64().unwrap()[..]);
        assert_eq!(t.version, 1);
        assert_eq!(t.key_idx, 1);
        assert_eq!(t.timestamp, 1466770783);
        assert_eq!(t.token_tag, None);
        assert_eq!(t.token_type, TokenType::Access);
        assert_eq!(t.lookup('u'), Some("6562d941-4f40-4db4-b96e-56a06d71c2c3"));
        assert_eq!(t.lookup('c'), Some("11019722839397809329"))
    }

    #[test]
    fn parse_user() {
        let t = Token::parse(USER_TOKEN).unwrap();
        assert_eq!(t.signature.0[..], "vpJs7PEgwtsuzGlMY0-Vqs22s8o9ZDlp7wJrPmhCgIfg0NoTAxvxq5OtknabLMfNTEW9amn5tyeUM7tbFZABBA==".from_base64().unwrap()[..]);
        assert_eq!(t.version, 1);
        assert_eq!(t.key_idx, 1);
        assert_eq!(t.timestamp, 1466770905);
        assert_eq!(t.token_tag, None);
        assert_eq!(t.token_type, TokenType::User);
        assert_eq!(t.lookup('u'), Some("6562d941-4f40-4db4-b96e-56a06d71c2c3"));
        assert_eq!(t.lookup('r'), Some("4feacc"))
    }

    #[test]
    fn parse_bot() {
        let t = Token::parse(BOT_TOKEN).unwrap();
        assert_eq!(t.signature.0[..], "-cEsTNb68hb-By81MZd5fF6NMDVzR_emkV_HfOnIdZTXsoeRRRZA7hmv9y2uLUNWDifNd-B8u0AjiAT_2rzUDg==".from_base64().unwrap()[..]);
        assert_eq!(t.version, 1);
        assert_eq!(t.key_idx, 1);
        assert_eq!(t.timestamp, -1);
        assert_eq!(t.token_tag, None);
        assert_eq!(t.token_type, TokenType::Bot);
        assert_eq!(t.lookup('p'), Some("cd57deb3-bab6-46fd-be28-a3d48ef2c6b7"));
        assert_eq!(t.lookup('b'), Some("b46833f9-ec2a-4c4a-8304-1a367f849467"));
        assert_eq!(t.lookup('c'), Some("ae3d1b9e-e47c-4e10-a751-e99a64ada74b"))
    }

    #[test]
    fn parse_session() {
        let t = Token::parse(SESSION_TOKEN).unwrap();
        assert_eq!(t.signature.0[..], "hmTE4dWsW3TuOXtvAuSuHMcEHxT4MCqGrJ2hCw0YLZ1_XnjSc3ByohekeSrz7zjmEzHM-QSkg8MbrawR-kcjBQ==".from_base64().unwrap()[..]);
        assert_eq!(t.version, 1);
        assert_eq!(t.key_idx, 1);
        assert_eq!(t.timestamp, 1466771315);
        assert_eq!(t.token_tag, Some("s"));
        assert_eq!(t.token_type, TokenType::User);
        assert_eq!(t.lookup('u'), Some("6562d941-4f40-4db4-b96e-56a06d71c2c3"));
        assert_eq!(t.lookup('r'), Some("4feacc"))
    }

    #[test]
    fn parse_provider() {
        let t = Token::parse(PROVIDER_TOKEN).unwrap();
        assert_eq!(t.signature.0[..], "qcJ9zxFHMaiqj-tauhywI435BBs8t6wFyXAShkSQqaHK9r36k012rJYJIE7TTCHlFaGOzsk6E7h5G8JkLVjFDg==".from_base64().unwrap()[..]);
        assert_eq!(t.version, 1);
        assert_eq!(t.key_idx, 1);
        assert_eq!(t.timestamp, 1467640768);
        assert_eq!(t.token_tag, None);
        assert_eq!(t.token_type, TokenType::Provider);
        assert_eq!(t.lookup('p'), Some("84fa6cbf-0845-42cf-93b5-1e2195c68e11"))
    }

    #[test]
    fn parse_legal_hold_access() {
        let t = Token::parse(LEGAL_HOLD_ACCESS_TOKEN).unwrap();
        assert_eq!(t.signature.0[..], "6wca6kIO7_SFAev_Pl2uS6cBdkKuGk6MIh8WBK_ivZnwtRVrXF2pEHiocUWQZDy8YTrEweTJrqxUDptA7M1SBA==".from_base64().unwrap()[..]);
        assert_eq!(t.version, 1);
        assert_eq!(t.key_idx, 1);
        assert_eq!(t.timestamp, 1558361639);
        assert_eq!(t.token_tag, None);
        assert_eq!(t.token_type, TokenType::LegalHoldAccess);
        assert_eq!(t.lookup('u'), Some("4763099d-ab9b-4720-a1a3-558877f8b3e2"));
        assert_eq!(t.lookup('c'), Some("17967041325642812284"))
    }

    #[test]
    fn parse_legal_hold_user() {
        let t = Token::parse(LEGAL_HOLD_USER_TOKEN).unwrap();
        assert_eq!(t.signature.0[..], "GsydW1LQvwGYBGFErvqcqJvcipumtcdfVL4Li83KwR1ucnm-IrPM40SKl9Rhsdv0sqF_MF_eyTqMe_XpXR81Cg==".from_base64().unwrap()[..]);
        assert_eq!(t.version, 1);
        assert_eq!(t.key_idx, 1);
        assert_eq!(t.timestamp, 1558361914);
        assert_eq!(t.token_tag, None);
        assert_eq!(t.token_type, TokenType::LegalHoldUser);
        assert_eq!(t.lookup('u'), Some("ca754009-bc1e-4ef7-8384-dfec056bcc97"));
        assert_eq!(t.lookup('r'), Some("64"))
    }

}
