# lint-openapi-regression

Detects backward-incompatible changes in OpenAPI JSON files by comparing a
candidate spec against historical baseline versions (e.g. `swagger-v5.json`,
`swagger-v6.json`).

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
lint-openapi-regression [OPTIONS] INPUT_FILE
```

### Options

| Flag               | Default                | Description                                          |
|--------------------|------------------------|------------------------------------------------------|
| `--baseline-dir`   | `services/brig/docs`   | Directory containing baseline `swagger-v*.json` files |
| `--update`         | off                    | Update the ignore file with new breaking changes     |
| `--api-version N`  | (none)                 | Compare only against the saved baseline of this version instead of all baselines |

### Examples

Check a new spec against all baselines in the default directory:

```bash
lint-openapi-regression services/brig/docs/swagger.json
```

Check against baselines in a custom directory:

```bash
lint-openapi-regression --baseline-dir path/to/baselines new-spec.json
```

Use an ignore file to suppress known violations:

```bash
lint-openapi-regression --ignore .lint-ignore.json services/brig/docs/swagger.json
```

Auto-update the ignore file with any new violations found:

```bash
lint-openapi-regression --ignore .lint-ignore.json --update services/brig/docs/swagger.json
```

Fetch the OpenAPI served for a specific version from a running brig and compare
it against the saved baseline of that same version (frozen-version drift check;
via `make openapi-lint-regression`):

```bash
make openapi-lint-regression VERSION=16
```

Note: `.lint-ignore.json`-style ignore files are keyed by baseline version and
record intentional changes *since* each version, so combining `--api-version`
with such a file masks frozen-version drift. Use a separate ignore file if you
need to suppress known drift.

## Exit codes

| Code | Meaning                                    |
|------|--------------------------------------------|
| 0    | No breaking changes (or all are ignored)   |
| 1    | Breaking changes detected                  |
| 2    | Input error (file not found, parse failure, version mismatch, missing baseline) |

## Ignore file format

The ignore file is a JSON object mapping baseline version keys to sets of route
identifiers (either `operationId` or rendered route like `GET /users/{_}`):

```json
{
  "v5": ["getUser", "POST /teams"],
  "v6": ["GET /users/{_}"]
}
```
