workspace(name = "wire_server")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

skylib_version = "0.8.0"

http_archive(
    name = "bazel_skylib",
    type = "tar.gz",
    url = "https://github.com/bazelbuild/bazel-skylib/releases/download/{}/bazel-skylib.{}.tar.gz".format(skylib_version, skylib_version),
    sha256 = "2ef429f5d7ce7111263289644d233707dba35e39696377ebab8b0bc701f7818e",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.11",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.11.tar.gz"],
    sha256 = "40fd6de12324b515042634ba13b02fa19f5c6e274eae6350be2e4d1e023fcd90",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
    "rules_haskell_toolchains",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:ghc_bindist.bzl",
    "haskell_register_ghc_bindists",
)

ghc_flags = ["-X" + ext for ext in [
    "AllowAmbiguousTypes",
    "BangPatterns",
    "ConstraintKinds",
    "DataKinds",
    "DefaultSignatures",
    "DerivingStrategies",
    "DeriveFunctor",
    "DeriveGeneric",
    "DeriveLift",
    "DeriveTraversable",
    "EmptyCase",
    "FlexibleContexts",
    "FlexibleInstances",
    "FunctionalDependencies",
    "GADTs",
    "InstanceSigs",
    "KindSignatures",
    "LambdaCase",
    "MultiParamTypeClasses",
    "MultiWayIf",
    "NamedFieldPuns",
    "NoImplicitPrelude",
    "OverloadedStrings",
    "PackageImports",
    "PatternSynonyms",
    "PolyKinds",
    "QuasiQuotes",
    "RankNTypes",
    "ScopedTypeVariables",
    "StandaloneDeriving",
    "TemplateHaskell",
    "TupleSections",
    "TypeApplications",
    "TypeFamilies",
    "TypeFamilyDependencies",
    "TypeOperators",
    "UndecidableInstances",
    "ViewPatterns",
]]

haskell_register_ghc_bindists(
    version = "8.4.4",
    compiler_flags = ghc_flags,
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    local_snapshot = "snapshot.yaml",
    packages = [
        "aeson",
        "ansi-terminal",
        "async",
        "attoparsec",
        "base",
        "base16-bytestring",
        "base64-bytestring",
        "bytestring",
        "bytestring-conversion",
        "case-insensitive",
        "clock",
        "configurator",
        "containers",
        "cookie",
        "cryptohash-md5",
        "cryptohash-sha1",
        "cql-io",
        "data-default",
        "deepseq",
        "errors",
        "exceptions",
        "extra",
        "hashable",
        "http-client",
        "http-client-tls",
        "http-types",
        "http-reverse-proxy",
        "immortal",
        "iproute",
        "monad-control",
        "mtl",
        "lens",
        "lens-datetime",
        "optparse-applicative",
        "unliftio",
        "unliftio-core",
        "unordered-containers",
        "pipes",
        "prometheus-client",
        "retry",
        "scientific",
        "singletons",
        "servant",
        "servant-client",
        "servant-server",
        "servant-swagger",
        "stm",
        "string-conversions",
        "streaming-commons",
        "swagger",
        "text",
        "tagged",
        "tasty",
        "time",
        "time-locale-compat",
        "tinylog",
        "transformers",
        "transformers-base",
        "unix",
        "uuid",
        "uri-bytestring",
        "vector",
        "wai",
        "wai-extra",
        "wai-middleware-prometheus",
        "wai-predicates",
        "wai-route",
        "wai-routing",
        "warp",
        "yaml",
    ],
)

