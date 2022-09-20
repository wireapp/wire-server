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

use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, Clone)]
pub struct Matcher {
    regex: Option<Regex>,
}

lazy_static! {
    static ref SLASHES: Regex = Regex::new("/+").unwrap();
    static ref DOUBLE_STAR_PATTERN: Regex = Regex::new(r#"\\\*\\\*"#).unwrap();
    static ref STAR_PATTERN: Regex = Regex::new(r#"\\\*"#).unwrap();
}

impl Matcher {
    pub fn new(items: &Vec<String>) -> Self {
        if items.len() == 0 {
            return Self { regex: None };
        }

        let items = items
            .iter()
            .map(|item| {
                let item = SLASHES.replace_all(item, "/");
                let item = item.trim_end_matches("/");
                let mut text = String::new();
                let pattern = regex::escape(item);
                let pattern = DOUBLE_STAR_PATTERN.replace_all(&pattern, ".*");
                let pattern = STAR_PATTERN.replace_all(&pattern, "[^/]+");

                text.push_str("(");
                text.push_str(&pattern);
                text.push_str(")");
                text
            })
            .collect::<Vec<_>>();

        let mut pattern = String::new();
        pattern.push_str("^(");
        pattern.push_str(&items.join("|"));
        pattern.push_str(")$");
        Self {
            regex: Some(Regex::new(&pattern).unwrap()),
        }
    }

    pub fn contains(&self, s: &str) -> bool {
        match &self.regex {
            None => false,
            Some(r) => {
                let s = SLASHES.replace_all(s, "/");
                let s = s.trim_end_matches("/");
                r.is_match(&s)
            }
        }
    }
}

// Tests ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut items = Vec::new();
        items.push("/foo".to_string());
        items.push("/foo/bar/baz".to_string());
        items.push("/x/y/".to_string());
        items.push("/i/**".to_string());
        items.push("/j/*".to_string());
        items.push("/k/v*".to_string());
        items.push("/a//c".to_string());
        let t = Matcher::new(&items);

        assert!(t.contains("/foo"));
        assert!(t.contains("/foo/bar/baz"));
        assert!(t.contains("/x/y"));
        assert!(!t.contains("/foo/bar"));
        assert!(t.contains("/a/c"));
        assert!(!t.contains("/a"));
        assert!(t.contains("/i/foo"));
        assert!(t.contains("/i/foo/zoo"));
        assert!(t.contains("/j/foo"));
        assert!(!t.contains("/j/foo/zoo"));
    }
}
