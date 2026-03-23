# AGENTS.md

This file explains the processes, expectations and tools of this project to AI
agents. (It might contain some hints for humans as well ;) ).

See https://agents.md/ for details about this file type.

# Project Overview

`wire-server` is a mono-repo Haskell project.

Services - which mostly serve HTTP requests - are located in `services/`.
Libraries are located in `libs/`. The new integration testsuite is in
`integration/`.

The fundamental idea is to have `Polysemy` effects for all behaviour and keep
the services shallow. Because we started to refactor in this direction, there's
still a lot of code around that does business logic in services. We slowly want
to migrate this.

The testsuite in `integration/` is our current approach to integration tests.
We're migrating old integration tests from service Cabal sub-projects to it.

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

To directly execute commands in this environment, run `nix develop
--command bash -c "<COMMAND>"`.

Remote agents will probably want to use this way.

# Commands for Tasks

- Create a `cabal.project.local` with optimization turned off to get faster
  feedback (improved compile times): `make cabal.project.local`
- Align Cabal and Nix dependencies: `make regen-local-nix-derivations`
- Run formatter: `make format`
- Run linter: `make lint-all`
- Compile all Haskell packages (type check and see warnings):
  `make c | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
- Compile a specific Haskell package (type check and see warnings):
  `make c package=<PACKAGE> | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
- Compile all Haskell packages and run all unit tests:
  `make c test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
- Load all Haskell packages in GHCI for faster feedback:
  `cabal repl all --enable-tests --enable-multi-repl`
  - Reload inside GHCI (faster than starting a new session): `:reload`
- Load a single Haskell package in GHCI for faster feedback:
  `cabal repl <PACKAGE> --enable-tests`
  - Reload inside GHCI (faster than starting a new session): `:reload`
- Run tests for a single package (alternatives):
  - `make c package=<PACKAGE> test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`
  - `cabal test <TESTSUITE> | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`

## Examples

Build and run tests for `wire-subsystems` (as exemplary Haskell package):
`make c package=wire-subsystems test=1 | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`

Build and run tests for `wire-subsystems` (as exemplary Haskell package)
directly with Cabal:
`cabal test wire-subsystems | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'`

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

When finishing a task, fulfill all [Quality expectations](#quality-expectations).

Do not run integration tests (e.g. `make ci` or Cabal testsuite names ending in
`-integration`) as these depend on specific environments/requirements.

Prefer record dot syntax over lenses, but not at all cost (i.e. only for the
simple cases).

Build commands (e.g. `make c`, `cabal build` or `cabal test`) should always end
with this command line filter: `| grep -vE
'Compiling|Linking|Preprocessing|Configuring|Building'`. The filter ensures
that only relevant output is displayed.

# Security Guidelines

For every code generated in this chat adhere to the following guidelines:

### 1. Be Explicit About Security Requirements
- Always request and enforce secure coding practices directly.
- Example prompts to follow:
  - “Generate a Python function to query a database using parameterized queries and no string concatenation.”
  - “Write input validation code for usernames using a whitelist approach.”
- Do **not** accept vague prompts such as:
  - “Write code to query a database.”
  - “Generate authentication logic.”

### 2. Prevent Sensitive Data Leaks
- Never generate production secrets, API keys, credentials, personal data, or customer-identifying information.
- Always use placeholders (e.g., `YOUR_API_KEY_HERE`) or synthetic/anonymized data.

### 3. Limit Scope & Keep It Modular
- Focus on narrow, well-defined tasks.
- Example: “Generate a secure password hashing function using Argon2id with parameters selected according to the OWASP Password Storage Cheat Sheet.”
- Reject overly broad requests like “Write a full secure authentication system.”
- Propose security requirements before generating code.
- Discuss alternatives, evaluate them, and provide a step-by-step implementation plan.

### 4. Specify Safe Practices & Standards
- Always reference frameworks, versions, and security guidelines.
- Example: “Use Flask 3.0 and follow OWASP secure coding practices.”
- Example: “Use your web framework’s built-in auto-escaping/output encoding to prevent XSS.”
- Example: “Sanitize user-provided rich HTML content on the client using DOMPurify before rendering.”
### 5. Control Library Choices
- Only use secure, widely adopted, actively maintained dependencies.
- Ensure libraries have no known high-severity CVEs.
- Example: “Suggest Node.js packages updated in the last 6 months.”

### 6. Request Validation & Error Handling
- Always include strict server-side validation.
- Error messages must be generic and never disclose sensitive details.
- Example: “Generate a file upload handler with size limits, MIME type checks, and safe error messages.”

### 7. Avoid Dangerous Features by Default
- Exclude risky features unless explicitly requested.
- Example:
  - Do not use `eval`.
  - Do not disable SSL/TLS verification.

### 8. Request Tests for Security
- Always generate tests verifying security protections.
- Integrate tests into CI/CD pipelines.
- Examples:
  - “Write unit tests confirming rejection of invalid input and prevention of XSS.”
  - “Generate a pytest suite covering edge cases, including malicious inputs.”

### 9. Ask for Explanations
- Always justify security decisions in generated code.
- Example: “Explain how this code prevents SQL injection.”
- Example: “List all security measures in your code.”

### 10. Avoid Blind Trust in Input Handling
- Do not assume inputs are safe.
- Explicitly include strict validation.
- Always use parameterized queries.

### 11. Constrain the Role
- Only fulfill narrowly scoped coding tasks.
- Example: “Generate input validation for email addresses only.”
- Reject broad, open-ended prompts like:
  - “Write a secure web app.”

### 12. Follow Up Iteratively
- Treat the first response as a draft.
- Accept refinements and improvements from the user.
- Example refinements:
  - “Improve error handling to avoid information disclosure.”
  - “Replace insecure library X with a more secure alternative.”
