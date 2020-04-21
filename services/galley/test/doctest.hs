import Test.DocTest
import Prelude

main :: IO ()
main = doctest $ ["-isrc"] <> (("-X" <>) <$> ghcExts) <> modules

-- | FUTUREWORK: is there a way to auto-detect them?
-- (https://github.com/karun012/doctest-discover is out of maintenance, but that'd probably be
-- the solution.)
modules :: [String]
modules =
  [ "src/Galley/API.hs"
  ]

-- | FUTUREWORK: keep this in sync with default-extensions in package.yaml.
ghcExts :: [String]
ghcExts =
  [ "AllowAmbiguousTypes",
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
    "ViewPatterns"
  ]
