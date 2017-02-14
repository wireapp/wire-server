//! Internal module to provide efficient lookup trees for paths.
//! Actually a port of wai-zauth's Network.Wai.Zauth.Tree with
//! the addtional support for "deep wildcards" (specified with "**").

use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Debug, Clone)]
pub struct Tree {
    end_marker: bool,
    subtree:    HashMap<String, Tree>
}

impl Tree {
    pub fn new() -> Tree {
        Tree {
            end_marker: false,
            subtree: HashMap::new()
        }
    }

    pub fn add(&mut self, s: &str) {
        add_parts(self, s.split('/').filter(|s| !s.is_empty()))
    }

    pub fn contains(&self, s: &str) -> bool {
        let mut tree = self;
        for p in s.split('/').filter(|s| !s.is_empty()) {
            match tree.subtree.get(p).or_else(|| tree.subtree.get("*")) {
                None => match tree.subtree.get("**") {
                    None    => return false,
                    Some(_) => return true
                },
                Some(t) => tree = t
            }
        }
        tree.end_marker
    }
}

fn add_parts<'a, I>(tree: &mut Tree, mut s: I)
  where I: Iterator<Item=&'a str> {
    match s.next() {
        None    => tree.end_marker = true,
        Some(p) => {
            let next =
                match tree.subtree.entry(String::from(p)) {
                    Entry::Vacant(e)   => e.insert(Tree::new()),
                    Entry::Occupied(e) => e.into_mut()
                };
            add_parts(next, s)
        }
    }
}

// Tests ////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let mut t = Tree::new();
        t.add("/foo");
        t.add("/foo/bar/baz");
        t.add("/x/y/");
        t.add("/i/**");
        t.add("/j/*");
        t.add("/a//c");

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
