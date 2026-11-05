# lint-openapi-regression

Detects backward-incompatible changes in OpenAPI specs. Two modes:

- **Single-version drift check** (`--api-version N`): compares the saved
  baseline snapshot `swagger-vN.json` against the spec that a running brig
  *live-renders* for version N (`GET /vN/api/swagger.json`). This catches
  regressions of old API versions that snapshots alone cannot see.
- **All-versions mode** (no `--api-version`): for every baseline snapshot
  (v5+), fetches the live spec of that same version and compares
  snapshot-vs-live per version.

## What it checks

| Check                              | Breaking direction |
|------------------------------------|--------------------|
| Route removed                      | Always             |
| Query parameter removed            | Always             |
| Required query parameter added     | Always             |
| Required body field added          | Request            |
| Response field removed (required)  | Response           |
| Enum value removed                 | Request            |
| Enum value added                   | Response           |

## Building

```bash
make c package=lint-openapi-regression
```

## Running tests

```bash
make c package=lint-openapi-regression test=1 \
  | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
```

Or directly with Cabal:

```bash
cabal test lint-openapi-regression-tests \
  | grep -vE 'Compiling|Linking|Preprocessing|Configuring|Building'
```

## CLI usage

```
lint-openapi-regression [OPTIONS] [INPUT_FILE]
```

### Options

| Flag               | Default                | Description                                          |
|--------------------|------------------------|------------------------------------------------------|
| `--baseline-dir`   | `services/brig/docs`   | Directory containing baseline `swagger-v*.json` files |
| `--brig-url`       | `http://localhost:8082`| Base URL of a running brig for live spec fetching (use `https:` for any non-local brig; `--update` should only run against a trusted brig since its response shapes the committed ignore files) |
| `--api-version N`  | (none)                 | Check only version N (baseline vN vs live vN spec)    |
| `INPUT_FILE`       | (none)                 | Offline candidate spec (only with `--api-version`)    |
| `--ignore FILE`    | (none)                 | Flat JSON ignore file (only with `--api-version`)     |
| `--ignore-dir DIR` | `.lint-ignore`         | Directory for per-version ignore files `vN.json`      |
| `--update`         | off                    | Write the current violations to the ignore file(s)    |

There is no default/implicit ignore file: violations are only suppressed via an
explicit `--ignore` file. `make openapi-lint-regression` passes
`.lint-ignore/vN.json` when it exists.

### Examples

Check every baseline version against the live specs served by brig:

```bash
lint-openapi-regression --brig-url http://localhost:8082
```

Check one version, applying its ignore file if present (via
`make openapi-lint-regression VERSION=15`):

```bash
lint-openapi-regression --brig-url http://localhost:8082 \
  --api-version 15 --ignore .lint-ignore/v15.json
```

Check an offline candidate file instead of fetching from brig:

```bash
lint-openapi-regression --api-version 15 new-spec.json
```

## Exit codes

| Code | Meaning                                    |
|------|--------------------------------------------|
| 0    | No breaking changes (or all are ignored)   |
| 1    | Breaking changes detected                  |
| 2    | Input error (file not found, parse failure, fetch failure, version mismatch, missing baseline) |

## Ignore file format

Ignore files are flat, per-version JSON arrays of route identifiers — either
an `operationId` or a rendered route like `GET /users/{_}`:

```json
[ "add-bot", "GET /users/{_}" ]
```

`--update` writes the **full current violation set** of each checked version
(ignored and unignored), so stale entries are pruned automatically: if a
suppressed violation disappears, the next `--update` run removes it.
