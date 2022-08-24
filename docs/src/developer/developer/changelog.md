# Changelog

The wire-server repo has a process for changelog editing that prevents
merge conflicts and enforces a consistent structure to the release
notes.

*Introduced in https://github.com/wireapp/wire-server/pull/1749.*

## tl;dr

Entries have to be written in individual files in a relevant subfolder of `./changelog.d/`.

*Example*: create the file `./changelog.d/2-features/potato-peeler` with one-line contents `Introduce automatic potato peeler functionality when buying potatoes, see [docs](link-to-docs)`

## Details

On every pull request, one is supposed to create a new file in the
appropriate subdirectory of `changelog.d`, containing just the text of
the corresponding changelog entry. There is no need to explicitly
write a PR number, because the `mk-changelog.sh` script will add it
automatically at the end. The name of the file does not matter, but
please try to make it unique to avoid unnecessary conflicts (e.g. use
the branch name).

It is still possible to write the PR number manually if so desired,
which is useful in case the entry should refer to multiple PRs. In
that case, the script leaves the PR number reference intact, as long
as it is at the very end of the entry (no period allowed afterwards!),
and in brackets. It is also possible to use the pattern `##` to refer
to the current PR number. This will be replaced throughout.

Multiline entries are supported, and should be handled
correctly. Again, the PR reference should either be omitted or put at
the very end. If multiple entries for a single PR are desired, one
should create a different file for each of them.

## Generating a CHANGELOG for a release

Just run the script `changelog.d/mk-changelog.sh` with no
arguments. It will print all the entries, nicely formatted, on
standard output. The script gets PR numbers from the `git` log. If
that fails for any reason (e.g. if an entry was added outside of a
PR), make sure that the entry has a manually specified PR number.
