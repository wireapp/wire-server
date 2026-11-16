{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | CI enforcement of the OpenAPI baseline regression check: the live-rendered
-- spec of every frozen API version (v5..v18) must remain backward compatible
-- with its committed baseline @services/brig/docs/swagger-v*.json@.
--
-- Frozen baselines of shipped versions never change; when this test fails, fix
-- the rendering change (or its accidental introduction) instead of editing the
-- baseline.  Violations on routes already accepted as drifted are suppressed
-- by the per-version ignore files in @test\/unit\/ignore\/@ (JSON arrays of
-- route identifiers, @LintOpenAPI.Ignore@ format); a violation on a route not
-- in the ignore file fails the test.  This is the compile-time twin of
-- @make openapi-lint-regression@, which additionally checks the live-rendered
-- spec of a running brig.
module Test.Wire.API.Routes.SwaggerRegression
  ( tests,
  )
where

import Data.Aeson qualified as A
import Data.FileEmbed (makeRelativeToProject)
import Data.Proxy (Proxy (..))
import Imports
import Language.Haskell.TH (litE, runIO, stringL)
import LintOpenAPI.Compare qualified as LintOpenAPI
import LintOpenAPI.Ignore qualified as LintOpenAPI
import LintOpenAPI.Parse qualified as LintOpenAPI
import Servant.OpenApi (HasOpenApi)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Wire.API.Routes.API (SpecialisedAPIRoutes)
import Wire.API.Routes.Public.Brig (BrigAPITag)
import Wire.API.Routes.Public.Brig.OAuth (OAuthAPITag)
import Wire.API.Routes.Public.Cannon (CannonAPITag)
import Wire.API.Routes.Public.Cargohold (CargoholdAPITag)
import Wire.API.Routes.Public.Galley (GalleyAPITag)
import Wire.API.Routes.Public.Gundeck (GundeckAPITag)
import Wire.API.Routes.Public.Spar (SparAPITag)
import Wire.API.Routes.Public.Swagger (versionedSwagger)
import Wire.API.Routes.Version

-- | @services\/brig\/docs\/swagger-v*.json@, the frozen OpenAPI baselines.
--
-- Under nix only this package's own directory is copied into the build sandbox,
-- so @nix\/wire-server.nix@ splices the baselines into
-- @test\/unit\/generated\/baselines@ and we prefer that copy; a plain cabal
-- build has the whole repository checked out and reads the real files instead.
-- The lookup is inline because a top-level splice cannot call a function
-- defined in the same module.
baselineDir :: FilePath
baselineDir =
  $( do
       spliced <- makeRelativeToProject "test/unit/generated/baselines"
       spliced' <- runIO (doesDirectoryExist spliced)
       fp <-
         if spliced'
           then pure spliced
           else makeRelativeToProject "../../services/brig/docs"
       litE (stringL fp)
   )

-- | Per-version ignore files (@swagger-ignore-v*.json@), suppressing violations
-- on routes already accepted as drifted.  These live in this package's test
-- tree (unlike the baselines, which belong to brig), so they are present both
-- in a plain checkout and inside the nix build sandbox without splicing.
ignoreDir :: FilePath
ignoreDir =
  $(litE . stringL =<< makeRelativeToProject "test/unit/ignore")

-- | Frozen, OpenAPI-3 baselines only: v0..v4 are Swagger 2.0, which the linter
-- does not support (@LintOpenAPI.Baseline.isOpenAPI3File@); v19 is the dev
-- version and has no baseline.
tests :: TestTree
tests =
  testGroup
    "OpenAPI regression (frozen baselines vs. live rendering)"
    [ checkVersion 5 (Proxy @'V5) (VersionNumber V5),
      checkVersion 6 (Proxy @'V6) (VersionNumber V6),
      checkVersion 7 (Proxy @'V7) (VersionNumber V7),
      checkVersion 8 (Proxy @'V8) (VersionNumber V8),
      checkVersion 9 (Proxy @'V9) (VersionNumber V9),
      checkVersion 10 (Proxy @'V10) (VersionNumber V10),
      checkVersion 11 (Proxy @'V11) (VersionNumber V11),
      checkVersion 12 (Proxy @'V12) (VersionNumber V12),
      checkVersion 13 (Proxy @'V13) (VersionNumber V13),
      checkVersion 14 (Proxy @'V14) (VersionNumber V14),
      checkVersion 15 (Proxy @'V15) (VersionNumber V15),
      checkVersion 16 (Proxy @'V16) (VersionNumber V16),
      checkVersion 17 (Proxy @'V17) (VersionNumber V17),
      checkVersion 18 (Proxy @'V18) (VersionNumber V18)
    ]

checkVersion ::
  forall v.
  ( HasOpenApi (SpecialisedAPIRoutes v BrigAPITag),
    HasOpenApi (SpecialisedAPIRoutes v GalleyAPITag),
    HasOpenApi (SpecialisedAPIRoutes v SparAPITag),
    HasOpenApi (SpecialisedAPIRoutes v CargoholdAPITag),
    HasOpenApi (SpecialisedAPIRoutes v CannonAPITag),
    HasOpenApi (SpecialisedAPIRoutes v GundeckAPITag),
    HasOpenApi (SpecialisedAPIRoutes v OAuthAPITag)
  ) =>
  Int ->
  Proxy v ->
  VersionNumber ->
  TestTree
checkVersion n pv vn = testCase ("v" <> show n) $ do
  baseline <-
    LintOpenAPI.parseOpenAPIFile (baselineDir </> ("swagger-v" <> show n <> ".json"))
      >>= either (assertFailure . ("parsing baseline: " <>)) pure
  candidate <-
    either assertFailure pure $
      LintOpenAPI.parseOpenAPIValue (A.toJSON (versionedSwagger pv vn))
  let violations = LintOpenAPI.compareSpecs baseline candidate
  ignored <-
    LintOpenAPI.readIgnoreFile (ignoreDir </> ("swagger-ignore-v" <> show n <> ".json"))
      >>= either assertFailure pure
  let unignored = filter (not . LintOpenAPI.isIgnored ignored) violations
      ignoredCount = length violations - length unignored
  unless (null unignored) $
    assertFailure $
      "live rendering of v"
        <> show n
        <> " is not backward compatible with the frozen baseline: "
        <> show unignored
        <> " (suppressing "
        <> show ignoredCount
        <> " already-ignored violations)"
        <> "\nFrozen baselines of shipped versions never change; fix the rendering change instead of editing the baseline."
        <> "\nIf (and only if) the drift is an accepted product decision, add the violating routes to "
        <> ("test/unit/ignore/swagger-ignore-v" <> show n <> ".json")
