# Local development setup on macOS

This page complements the [build instructions](building.md) with considerations
specific to macOS, in particular Apple Silicon, where wire-server is not fully
arm64-compatible and some builds or docker images may need Rosetta 2 or
`platform linux/amd64`. Treat this as
a list of things to keep in mind rather than step-by-step instructions.

## direnv + nix

direnv runs `.envrc` in bash without sourcing your shell profile, so `nix` may
not be on `PATH`. Fix it in `~/.config/direnv/direnvrc`:

```bash
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
```

## Nix binary caches

Sometimes the Nix daemon does not read `~/.config/nix/nix.conf` and the
"not a trusted user" error shows up — common with Determinate Nix, where
user-level configuration is not enough. Fix it by adding the caches
system-wide in `/etc/nix/nix.custom.conf` and restarting the daemon:

```conf
extra-substituters = https://cache.nixos.org https://nix-build.zinfra.io
extra-trusted-public-keys = <one public key per substituter>
trusted-users = root <your-user>
```

Then restart the daemon:

```bash
sudo launchctl kickstart -k system/systems.determinate.nix-daemon
```

For https://nix-build.zinfra.io the public key can be found in the [build instructions](building.md) on the nix section.

For `https://cache.nixos.org` the default key can be found in
[`trusted-public-keys`](https://nixos.org/manual/nix/stable/command-ref/conf-file.html#conf-trusted-public-keys)
in the Nix manual.

## Personal overrides: `.envrc.local`

`.envrc.local` is git ignored and loaded at the end of `.envrc`. 

Typical macOS overrides include a longer timeout for the port checks in `run-services`
(the default is too short on macOS and causes false negatives). 

Also a patched
nginx earlier on `PATH`. The nix-provided nginx (nginz) is linked against
[libzauth](https://github.com/wireapp/wire-server/tree/develop/libs/libzauth),
Wire's authentication library that lets nginx validate Wire access tokens at
the proxy layer. Mac OS dynamic linker requires absolute install names and
fails with `Library not loaded`, so the workaround is a patched copy of the
nginx binary. 

You may also need `REDIS_PASSWORD` for the Redis workaround further down:

```bash
export TEST_TIMEOUT_SECONDS=30
export PATH="$HOME/.wire-dev/bin:$PATH"
export REDIS_PASSWORD=very-secure-redis-master-password
```


## nginz: patching the nginx binary

The nginx binary from the nix store refers to the `libzauth` dylib by a
relative path that macOS' `dyld` cannot resolve. A patched copy placed earlier
on `PATH` fixes this:

```bash
mkdir -p ~/.wire-dev/bin
cp /nix/store/<nginx-hash>-nginx-<version>/bin/nginx ~/.wire-dev/bin/nginx
chmod +w ~/.wire-dev/bin/nginx
install_name_tool -change \
  "target/lib/libzauth-<hash>.dylib" \
  "/nix/store/<libzauth-hash>-rust_zauth-c-<version>-lib/lib/libzauth-<hash>.dylib" \
  ~/.wire-dev/bin/nginx
codesign -s - -f ~/.wire-dev/bin/nginx
```

Run `otool -L` on the nginx binary from the store to find the exact paths.
Nix store hashes change with nix upgrades, so the patch may need repeating.

## Redis cluster workaround

The Redis cluster started by
`deploy/dockerephemeral/docker-compose.yaml` advertises Docker-internal IPs
which are not routable from the macOS host (gundeck fails with
`ConnectTimeout`). 

Workaround: point gundeck at the single Redis master in `services/gundeck/gundeck.integration.yaml` (plus the matching `REDIS_PASSWORD`
in `.envrc.local`):

```yaml
redis:
  host: 127.0.0.1
  port: 6379
  connectionMode: master
  enableTls: false
  insecureSkipVerifyTls: false
```

The password for the single master is `very-secure-redis-master-password`
(distinct from the cluster password), set via `REDIS_PASSWORD` in
`.envrc.local`.
