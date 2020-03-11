This directory contains [custom Stack snapshots][custom] used for Wire code.

[custom]: https://docs.haskellstack.org/en/stable/custom_snapshot/

Snapshot definitions should never be changed (once committed to `develop`), because in other
repositories we refer to snapshot definitions by URL.  This goes for *ANY* change!  What
matters is that the sha256 hash of the file remains intact!

(Rationale: Stack only downloads snapshot definitions once, and never checks whether they have
changed. If a snapshot changes and you have a repo that depends on it, you will get
inconsistent results depending on whether you've built that repo before or not.)

To add, modify, or remove packages, a new snapshot should be created. It can be based on the
previous snapshot version. For major changes, e.g. LTS bumps, it's better to create a snapshot
from scratch.

Some packages in this snapshot reference tar files instead of Git repos. This is due to
several issues in Stack that make working with big Git repositories unpleasant:

  * https://github.com/commercialhaskell/stack/issues/4345
  * https://github.com/commercialhaskell/stack/issues/3551

Unless the fixes to those are released, it's recommended to use GitHub's tar releases for
packages with big repos.
