import Test.DocTest

main :: IO ()
main = doctest $ ["-isrc"] <> (("-X" <>) <$> ghcExts) <> modules

-- | FUTUREWORK: is there a way to auto-detect them?
-- (https://github.com/karun012/doctest-discover is out of maintenance, but that'd probably be
-- the solution.)
modules :: [String]
modules =
  [ "src/Web/Scim/Server/Mock.hs",
    "src/Web/Scim/Test/Util.hs"
  ]

-- | FUTUREWORK: keep this in sync with default-extensions in package.yaml.
ghcExts :: [String]
ghcExts =
  [ "ConstraintKinds",
    "DataKinds",
    "DeriveFunctor",
    "DeriveGeneric",
    "FlexibleContexts",
    "FlexibleInstances",
    "KindSignatures",
    "LambdaCase",
    "MultiParamTypeClasses",
    "OverloadedStrings",
    "RankNTypes",
    "ScopedTypeVariables",
    "TypeApplications",
    "TypeFamilies",
    "TypeOperators",
    "TypeSynonymInstances"
  ]
