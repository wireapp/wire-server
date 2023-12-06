#!/usr/bin/env -S nix -Lv run github:wireapp/ghc-flakr/ecb1f45f1549e06c92d71164e305ce501eb0e36e
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Turtle

main = do
  (release, repo) <- options "Upload boms" do
    (,)
      <$> optText "release" 't' "Which release tag to upload the artifacts to"
      <*> optText "repo" 'r' "Which repository to upload the artifacts to"
  let bomName = "wire-server-bom-" <> release <> ".json"
  ExitSuccess <- proc "nix" ["build", "-f", "nix", "wireServer.allLocalPackagesBom", "-o", bomName] mempty
  printf ("uploading " % s % " to release " % s % "\n") bomName ("chart/" <> release)
  proc "gh" ["-R", repo, "release", "upload", "chart/" <> release, bomName] mempty
