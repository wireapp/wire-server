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
    proc
      "wget"
      ["https://github.com/wireapp/wire-server/archive/refs/tags/chart/" <> release <> ".zip"]
      mempty
    proc "unzip" [release] mempty
    bomName <- ("wire-server-bom-" <>) . T.pack . show . nominalDiffTimeToSeconds <$> getPOSIXTime
    let bomPath = "./" <> bomName <> ".json"
    ExitSuccess <-
      proc
        "nix"
        [ "build",
          "-f",
          "wire-server-chart-" <> release <> "/nix",
          "wireServer.allLocalPackagesBom",
          "-o",
          bomPath
        ]
        mempty
    printf ("uploading " % s % " to release" % s % "\n") bomName release
    proc "gh" ["release", "upload", release, bomPath] mempty
  pure ()
