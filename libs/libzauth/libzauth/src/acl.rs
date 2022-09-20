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

use asexp::Sexp;
use matcher::Matcher;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Error {
    Parse(&'static str),
}

pub type AclResult<A> = Result<A, Error>;

#[derive(Debug, Clone)]
pub struct Acl {
    acl: HashMap<String, List>,
}

impl Acl {
    pub fn new() -> Acl {
        Acl {
            acl: HashMap::new(),
        }
    }

    pub fn from_str(s: &str) -> AclResult<Acl> {
        let sexp = Sexp::parse_toplevel(s);
        match sexp {
            Err(()) => Err(Error::Parse("invalid s-expressions")),
            Ok(sexp) => Acl::from_sexp(&sexp),
        }
    }

    fn from_sexp(s: &Sexp) -> AclResult<Acl> {
        match *s {
            Sexp::Map(ref entries) => {
                let mut acl = HashMap::new();
                for &(ref key, ref list) in entries {
                    if let Some(k) = key.get_str().map(String::from) {
                        acl.insert(k, List::from_sexp(&list)?);
                    } else {
                        return Err(Error::Parse("not a string"));
                    }
                }
                Ok(Acl { acl })
            }
            _ => Err(Error::Parse("expected key and values")),
        }
    }

    pub fn allowed(&self, key: &str, path: &str) -> bool {
        self.acl
            .get(key)
            .map(|list| match *list {
                List::Black(ref t) => !t.contains(path),
                List::White(ref t) => t.contains(path),
            })
            .unwrap_or(false)
    }
}

#[derive(Debug, Clone)]
enum List {
    Black(Matcher),
    White(Matcher),
}

impl List {
    fn from_sexp(s: &Sexp) -> AclResult<List> {
        let items = match *s {
            Sexp::Tuple(ref a) => a.as_slice(),
            Sexp::Array(ref a) => a.as_slice(),
            _ => return Err(Error::Parse("s-expr not a list")),
        };

        if items.is_empty() {
            return Err(Error::Parse("list is empty"));
        }

        match items[0].get_str() {
            Some("blacklist") => List::items(&items[1..]).map(List::Black),
            Some("whitelist") => List::items(&items[1..]).map(List::White),
            _ => Err(Error::Parse("'blacklist' or 'whitelist' expected")),
        }
    }

    fn items(xs: &[Sexp]) -> AclResult<Matcher> {
        let items: AclResult<Vec<_>> = xs.iter().map(List::read_path).collect();
        let m = Matcher::new(&items?);
        Ok(m)
    }

    fn read_path(s: &Sexp) -> AclResult<String> {
        match *s {
            Sexp::Tuple(ref a) | Sexp::Array(ref a) if a.len() == 2 => {
                match (a[0].get_str(), a[1].get_str()) {
                    (Some("path"), Some(x)) => Ok(String::from(x)),
                    _ => Err(Error::Parse("'path' not found")),
                }
            }
            _ => return Err(Error::Parse("s-expr not a list")),
        }
    }
}

// Tests ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    const ACL: &'static str = r#"
        a (blacklist (path "/forbidden/*") (path "/foo"))

        u (blacklist (path "/x/*/z")
                     (path "/a/**"))

        b (whitelist (path "/conversation/message")
                     (path "/foo/bar/*"))

        # this is a comment that should not lead to a parse failure.
        la (whitelist (path "/legalhold/**"))

        x (blacklist)

        y (whitelist)
        "#;

    #[test]
    fn read() {
        let acl = Acl::from_str(ACL).unwrap();
        assert!(acl.allowed("a", "/hello"));
        assert!(acl.allowed("a", "/forbidden"));
        assert!(!acl.allowed("a", "/forbidden/kingdom"));
        assert!(!acl.allowed("a", "/foo"));
        assert!(!acl.allowed("u", "/x/y/z"));
        assert!(!acl.allowed("u", "/x/here/z"));
        assert!(acl.allowed("u", "/x/here/z/x"));
        assert!(acl.allowed("b", "/conversation/message"));
        assert!(acl.allowed("b", "/foo/bar/baz"));
        assert!(!acl.allowed("b", "/foo/bar/"));
        assert!(!acl.allowed("b", "/anywhere/else/"));
        assert!(acl.allowed("x", "/everywhere"));
        assert!(acl.allowed("x", "/"));
        assert!(!acl.allowed("y", "/nowhere"));
        assert!(acl.allowed("la", "/legalhold/something"));
        assert!(!acl.allowed("la", "/mistyped/something"));
    }
}
