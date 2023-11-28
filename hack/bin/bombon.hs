#!/usr/bin/env -S nix -Lv run github:wireapp/ghc-flakr/ecb1f45f1549e06c92d71164e305ce501eb0e36e
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Time
import Data.Time.Clock.POSIX
import Turtle

main = do
  release <- options "Upload boms" do
    optText "release" 't' "Which release tag to upload the artifacts to"

  with (mktempdir "." "tmp") \tmpDir -> do
    cd tmpDir
    let git l = proc "git" l mempty
    git ["clone", "https://github.com/wireapp/wire-server", "."]
    git ["checkout", release]
    git ["submodule", "update", "--init", "--recursive"]
    bomName <- ("wire-server-bom-" <>) . T.pack . show . nominalDiffTimeToSeconds <$> getPOSIXTime
    let bomPath = "./" <> bomName <> ".json"
    ExitSuccess <-
      proc
        "nix"
        [ "build",
          "-f",
          "nix",
          "wireServer.allLocalPackagesBom",
          "-o",
          bomPath
        ]
        mempty
    printf ("uploading " % s % " to release " % s % "\n") bomName ("chart/" <> release)
    proc
      "gh"
      [ "-R",
        "wireapp/wire-server",
        "release",
        "upload",
        "chart/" <> release,
        bomPath
      ]
      mempty
  pure ()
