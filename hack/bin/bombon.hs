#!/usr/bin/env -S nix -Lv run github:wireapp/ghc-flakr/ecb1f45f1549e06c92d71164e305ce501eb0e36e
{-# language BlockArguments, OverloadedStrings #-}

import Turtle
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Data.Time

main = do
  nixDir <- options "Upload boms" do
    optText "nixDir" 'd' "Where to find the nix code"
  with (mktempdir "." "tmp") \tmpDir -> do
    bomName <- ("wire-server-bom-" <>) . T.pack . show . nominalDiffTimeToSeconds <$> getPOSIXTime
    let bomPath = T.pack tmpDir <> "/" <> bomName
    proc
      "nix"
      ["build", "-f", nixDir, "wireServer.allLocalPackagesBom", "-o", bomPath]
      mempty
    printf ("Copying "%s%" to s3\n") bomName
    proc "aws" ["s3", "cp", bomPath, "s3://wire-server-bom/" <> bomName] mempty
  pure ()
