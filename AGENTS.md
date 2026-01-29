# AGENTS.md

This file explains the processes, expectations and tools of this project to AI
agents. (It might contains some hints for humans as well ;) ).

See https://agents.md/ for details about this file type.

# Development Environment

The development environment is based on Nix flakes. Entrypoint is the
`flake.nix` file. More Nix related code is in `nix/`.

## Local

All tools and dependencies are provided by the `direnv` environment. I.e. if
you're on a local machine (e.g. developer laptop) and the current working
directory is inside this project directory, then all required tools and
dependencies are automatically available in the environment.

Local agents (e.g. copilot-cli) will probably want to use this way.

## Cloud / non-direnv

In cloud environments or when `direnv` is not installed, `nix develop` starts a
shell with all required tools and dependencies.

To directly execute commands in this environment, run `nix flake
--command bash -c "<COMMAND>"`.

Remote agents will probably want to use this way.

# Commands for Tasks

- Create a `cabal.project.local` with optimization turned off to get faster
- Align Cabal and Nix dependencies: `make regen-local-nix-derivations`
- Run formatter: `make format`
- Run linter: `make lint-all`
- Compile all Haskell packages (type check and see warnings): `make c`
- Compile a specific Haskell package (type check and see warnings): `make c package=<PACKAGE>`
- Compile all Haskell packages and run all unit tests `make c test=1`
  feedback (compile times): `make cabal.project.local`
- Load all Haskell packages in GHCI for faster feedback: `cabal repl all --enable-tests --enable-multi-repl`
  - Reload inside GHCI (faster than starting a new session): `:reload`
- Load a single Haskell package in GHCI for faster feedback: `cabal repl <PACKAGE> --enable-tests`
  - Reload inside GHCI (faster than starting a new session): `:reload`
- Run tests for a single package (alternatives):
  - `make c package=<PACKAGE> test=1`
  - `cabal test <TESTSUITE>`

# Quality expectations

- Ensure that the whole project type checks
- Ensure that there are no compiler warnings
- Ensure that all unit tests are "green" (executed successfully) by
  running them
- Ensure that Cabal and Nix dependencies are aligned
- Ensure that the code is formatted according to our formatting rules

# Agent commandments

Ensure these after every change:

- Haskell code still type checks
- Unit tests of all changed packages are green

When finishing a task, fulfill all [Quality expectations](#Quality expectations)
