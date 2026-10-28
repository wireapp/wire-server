# Running wire-server locally on Apple Silicon (arm64) macOS

This repo's docs assume an amd64 dev machine (see the top-level README). On an
Apple Silicon Mac the standard flow (`direnv allow` + `./deploy/dockerephemeral/run.sh`
+ `make cr`) hits a few portability bugs. This note has the exact commands and
the fixes that made it work, so you don't have to rediscover them.

## Prerequisites (one-time)

- `direnv`, `nix` (with flakes), `docker`/OrbStack, all already set up per the
  main [building.md](./building.md).
- Rosetta installed (`softwareupdate --install-rosetta` if `arch -x86_64 /usr/bin/true`
  fails) — needed to run amd64-only container images under emulation.
- These source fixes applied (already committed if you're on a branch that has
  them; otherwise re-apply):
  - `libs/http2-manager/http2-manager.cabal`: `cpp-options: -DENABLE-TRAILING-DOT-TEST`
    → `-DENABLE_TRAILING_DOT_TEST` (hyphen isn't a legal C macro name; clang on
    macOS errors on it, gcc on Linux CI silently tolerates it).
  - `hack/bin/cabal-install-artefacts.sh`: rewritten to avoid GNU-only `xargs -i`
    and a BSD-xargs crash when the same `{}` placeholder is used twice in one
    `-I` command (`xargs: command line cannot be assembled, too long`). Now
    uses `xargs -I {} sh -c '...' _ {}` with `$1`/`$2` instead.

## 1. Enter the dev shell

```bash
cd wire-server
direnv allow   # first time only; builds/fetches the nix devShell (~10 min,
               # only ghc-9.10.3-with-packages + amazonka + amazonka-s3-streaming
               # need local compilation on aarch64-darwin, everything else is cached)
```

Everything below assumes you're either `cd`'d in with direnv active, or you
prefix commands with `direnv exec .`.

## 2. Start backing services (Cassandra, ES, Redis, RabbitMQ, Postgres, fake AWS)

Do **not** just run `./deploy/dockerephemeral/run.sh` directly — on macOS it
fails/tears itself down for two reasons:

1. `quay.io/wire/localstack:0.0.9` has no arm64 image →
   `no matching manifest for linux/arm64/v8`.
2. The script ends with `sleep infinity` to block in the foreground; macOS's
   BSD `sleep` doesn't support `infinity`, so it errors immediately, and the
   script's `trap cleanup EXIT` tears every container back down right after
   declaring success.

Workaround — run the `docker-compose up -d` step directly instead of the whole
script, forcing amd64 images (OrbStack/Docker emulate them fine via Rosetta):

```bash
direnv exec . bash -c '
  export DOCKER_DEFAULT_PLATFORM=linux/amd64
  docker-compose --file deploy/dockerephemeral/docker-compose.yaml up -d
'
```

First run pulls a lot of amd64 image layers (slow); subsequent runs are fast.
Check everything is healthy:

```bash
docker ps --format '{{.Names}}\t{{.Status}}'
```

To stop these later:

```bash
direnv exec . bash -c '
  export DOCKER_DEFAULT_PLATFORM=linux/amd64
  docker-compose --file deploy/dockerephemeral/docker-compose.yaml down
'
```

## 3. Build + run the services

```bash
ulimit -n 10240   # or `ulimit 10240`; raises fd limit for the big build
direnv exec . make cr
```

`make cr` = `make c` (compile everything) + `db-migrate` + `./dist/run-services`
(launches brig, galley, gundeck, cannon, cargohold, proxy, spar — each twice,
once for `example.com` and once for `b.example.com`, to support local
federation testing). This call blocks in the foreground; run it in a
background/dedicated terminal.

First full build compiles ~hundreds of packages from scratch (optimizations
are off via the repo's `cabal.project.local`, so it's as fast as cabal gets).
Subsequent runs are incremental and much faster.

## 4. Check it's alive

```bash
for p in 8082 8083 8084 8085 8086 8087 8089 8091; do
  curl -s -o /dev/null -w "port $p -> %{http_code}\n" http://127.0.0.1:$p/i/status
done
```
All should return `200`.

Port map (one process pair each, see `/tmp/<service>-<domain>-*.yaml` for the
exact generated configs):

| service     | port |
|-------------|------|
| brig        | 8082 |
| cannon      | 8083 |
| cargohold   | 8084 |
| galley      | 8085 |
| gundeck     | 8086 |
| proxy       | 8087 |
| spar        | 8091 |

Live OpenAPI docs: `http://127.0.0.1:8082/api/swagger.json`.

Note: `./dist/run-services` DOES also start `nginz` (the public reverse proxy
on port 8080/8443 that validates `Z-Auth` tokens into `Z-User` headers) — it's
compiled as part of the normal nix devShell, no separate build step needed.
But on macOS it needs one more env var or it crashes on launch (see below).
Until that's set, you can still call endpoints directly without nginz: hit
`/i/...` internal endpoints (no auth), or after `/login` pass the user id
yourself as `Z-User: <uuid>` instead of a bearer token.

### Making nginz actually start (macOS only)

Symptom in the `run-services` output:
```
[nginz@example.com] dyld[...]: Library not loaded: target/lib/libzauth-<hash>.dylib
```
nginz's compiled-in zauth C module dynamically links the Rust-built
`libzauth`, and the install name baked in at Nix build time is a relative
path only valid inside the build sandbox — it doesn't exist at runtime.

Fix: locate the actual `.dylib` in the Nix store and point `DYLD_LIBRARY_PATH`
at its directory:
```bash
find /nix/store -maxdepth 2 -iname '*rust_zauth-c*-lib' 2>/dev/null
# e.g. /nix/store/5w1cbzzkyrxg9jvk0084aqzg7x8p0rq3-rust_zauth-c-3.0.0-lib
ls <that path>/lib   # confirm libzauth-<hash>.dylib is in there
```

**Important**: you can't just `export DYLD_LIBRARY_PATH=...` before `make cr`
— this repo's Makefile sets `SHELL := /usr/bin/env bash`, and `/usr/bin/env`
is a SIP-protected Apple system binary. macOS's dyld strips all `DYLD_*`/
`LD_*` env vars at that exec hop, so anything you exported is already gone by
the time `bash`/`run-services`/`nginx` run, with no error from `make` itself.

Instead, let `make` do the build/migrate, then launch the services binary
directly yourself (bypassing make's recipe shell) with the var set:
```bash
direnv exec . make c db-migrate   # build + migrate only, no launch
direnv exec . bash -c '
  export DYLD_LIBRARY_PATH="/nix/store/5w1cbzzkyrxg9jvk0084aqzg7x8p0rq3-rust_zauth-c-3.0.0-lib/lib"
  exec ./dist/run-services
'
```
Confirm: `curl http://127.0.0.1:8080/api/swagger.json` → 200, and
`ps aux | grep nginx` shows a master + worker processes.

If a previous `run-services` crashed (e.g. "timeout waiting for port to be
free"), it won't have reaped its own children — kill stray `dist/*` processes
by PID before relaunching rather than just retrying.

## 6. Running wire-webapp against this local backend

Most frontend devs point `wire-webapp` at a shared dev/staging backend, so its
`.env` is likely already set up that way — check before editing:

```bash
grep BACKEND_REST /Users/osama.salama/Documents/wire/wire-webapp/.env
```

To point it at the local backend instead (back up first):
```bash
cd /Users/osama.salama/Documents/wire/wire-webapp
cp .env .env.bak   # restore with `cp .env.bak .env` when done
```
Then edit `.env`:
- Comment out whatever `BACKEND_REST`/`BACKEND_WS` block is currently active
  (staging/dev-server/production).
- Uncomment (or add) the local block:
  ```
  BACKEND_REST="http://localhost:8080"
  BACKEND_WS="ws://localhost:8080"
  APP_BASE="http://localhost:8081"
  ```
- Also fix the `APP_BASE=` line near the top of the file to match — dotenv
  keeps the *first* occurrence of a duplicate key, so a stale value up top
  silently wins over one set further down.

Run it (Yarn is vendored, no global install needed):
```bash
node .yarn/releases/yarn-4.17.0.cjs start   # nx serve server, backgrounds a webpack dev server
```
First compile takes a minute or two (webpack). Once you see `webpack ...
compiled`, check `curl http://localhost:8081/` → 200, then open
`http://localhost:8081` in a browser. `ENFORCE_HTTPS="false"` in `.env` means
plain `http://` works fine locally, no mkcert/custom-hosts setup needed.

## 7. Stopping everything

```bash
# kill the wire-server service processes (run-services' children get
# reparented, so pkill by binary name rather than the parent pid)
pkill -f 'dist/brig '
pkill -f 'dist/galley '
pkill -f 'dist/gundeck '
pkill -f 'dist/cannon '
pkill -f 'dist/cargohold '
pkill -f 'dist/proxy '
pkill -f 'dist/spar '
pkill -f 'dist/background-worker'

# stop the backing containers
direnv exec . bash -c '
  export DOCKER_DEFAULT_PLATFORM=linux/amd64
  docker-compose --file deploy/dockerephemeral/docker-compose.yaml down
'
```
