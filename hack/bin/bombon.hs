#!/usr/bin/env -S nix -Lv run github:wireapp/ghc-flakr/ecb1f45f1549e06c92d71164e305ce501eb0e36e
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Turtle

main = do
  release <- options "Upload boms" do
    optText "release" 't' "Which release tag to upload the artifacts to"

  with (mktempdir "." "tmp") \tmpDir -> do
    cd tmpDir
    let git l = proc "git" l mempty
    git ["clone", "https://github.com/mangoiv/wire-server", "."]
    git ["checkout", release]
    git ["submodule", "update", "--init", "--recursive"]
    let bomName = "wire-server-bom-" <> release <> ".json"
    ExitSuccess <-
      proc
        "nix"
        [ "build",
          "-f",
          "nix",
          "wireServer.allLocalPackagesBom",
          "-o",
          bomName
        ]
        mempty
    printf ("uploading " % s % " to release " % s % "\n") bomName ("chart/" <> release)
    proc
      "gh"
      [ "-R",
        "mangoiv/wire-server",
        "release",
        "upload",
        "chart/" <> release,
        bomName
      ]
      mempty
  pure ()
