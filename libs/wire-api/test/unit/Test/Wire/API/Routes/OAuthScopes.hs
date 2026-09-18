{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

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

-- | Two independent places declare which OAuth scope an endpoint needs, and
-- nothing keeps them in sync:
--
-- 1. @charts/nginz/values.yaml@ -- @oauth_scope:@ on an upstream entry.  This is
--    what is actually /enforced/: nginz rejects OAuth tokens without the scope.
-- 2. The servant routing tables -- 'Wire.API.Routes.Public.DescriptionOAuthScope'.
--    This is only /documentation/: it appends a line to the endpoint description
--    in the swagger docs and has no effect on request handling.
--
-- Forgetting (2) while doing (1) -- or, more commonly, adding a new version of an
-- endpoint that is already covered by (1) and not carrying the annotation over --
-- silently produces endpoints that reject OAuth tokens for a scope documented
-- nowhere.  This module compares the two for the development version, which is
-- the only one still assembled from the routing tables.
module Test.Wire.API.Routes.OAuthScopes (tests) where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Conversion (toByteString')
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Yaml
import Imports
import Language.Haskell.TH (runIO)
import Servant.API (toUrlPiece)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Regex.TDFA ((=~))
import Wire.API.Routes.Public (renderOAuthScope)
import Wire.API.Routes.Public.Swagger (devVersion, devVersionSwagger)
import Wire.API.Routes.Version

tests :: TestTree
tests =
  testGroup
    "OAuth scopes (charts/nginz/values.yaml vs. swagger docs)"
    [ testCase "nginz path patterns avoid PCRE-only constructs" testPatternVocabulary,
      testCase "enforced scopes and documented scopes agree" testScopesAgree
    ]

--------------------------------------------------------------------------------
-- what nginz enforces

-- | The locations nginz emits, in the order it emits them.
--
-- @charts/nginz/templates/_helpers.tpl@ merges @upstreams@ (minus
-- @ignored_upstreams@) with the enabled @extra_upstreams@ into a single map, and
-- @templates/conf/_nginx.conf.tpl@ ranges over that map.  Go template map
-- iteration is sorted by key, so upstreams are emitted alphabetically and only
-- the list within one upstream keeps its document order -- which is exactly what
-- decoding into a 'Map' and taking 'Map.elems' gives us.
newtype NginzLocations = NginzLocations [Location]

data Location = Location
  { locPattern :: Text,
    locOldScope :: Maybe Text,
    locNewScopes :: [Text]
  }

-- | The scopes that get an OAuth token past nginz to this endpoint.
--
-- Empty when nginz requires no scope, and also when it requires one
-- not in 'Wire.API.OAuth.OAuthScopes'.  Mistyped scope names are
-- caught by 'testScopeNamesAreReal'.
enforcedScope :: Text -> Text -> Maybe Text
enforcedScope method path = do
  loc <- find locationMatches nginzLocations
  case loc.locOldScope of
    Just s -> Just (oldMethodScopeTier method <> ":" <> s)
    Nothing -> find (newMethodScopeTier method `T.isPrefixOf`) loc.locNewScopes
  where
    -- Does this location capture that path?  nginx anchors regex locations at the
    -- start of the URI but not at the end, so a pattern without a trailing @$@
    -- matches every path with that prefix.
    --
    -- The patterns are PCRE (that is what nginx uses) and we match them with
    -- regex-tdfa, which is POSIX ERE.  The two agree on the handful of constructs
    -- values.yaml actually uses; 'testPatternVocabulary' keeps it that way.
    locationMatches :: Location -> Bool
    locationMatches loc =
      T.unpack (probePath path) =~ T.unpack ("^" <> locPattern loc)

    -- @/conversations/{cnv}/code@ becomes @/conversations/PARAM/code@: the literal
    -- segments still have to match, the captures must not.
    probePath :: Text -> Text
    probePath t =
      let (before, rest) = T.breakOn "{" t
       in if T.null rest
            then before
            else before <> "PARAM" <> probePath (T.drop 1 (T.dropWhile (/= '}') rest))

    oldMethodScopeTier :: Text -> Text
    oldMethodScopeTier "GET" = "read"
    oldMethodScopeTier "POST" = "write"
    oldMethodScopeTier "PUT" = "write"
    oldMethodScopeTier "DELETE" = "admin"

    newMethodScopeTier :: Text -> Text
    newMethodScopeTier "GET" = "read"
    newMethodScopeTier "POST" = "write-only"
    newMethodScopeTier "PUT" = "write-only"
    newMethodScopeTier "DELETE" = "delete-only"

nginzLocations :: [Location]
nginzLocations =
  case Yaml.decodeEither' nginzValues of
    Left e -> error $ "charts/nginz/values.yaml: " <> Yaml.prettyPrintParseException e
    Right (NginzLocations ls) -> ls

-- | @charts\/nginz\/values.yaml@, embedded at compile time.
--
-- Under nix only this package's own directory is copied into the build sandbox,
-- so @nix\/wire-server.nix@ splices the chart into @test\/unit\/generated\/@ and
-- we prefer that copy; a plain cabal build has the whole repository checked out
-- and reads the real file instead.  The lookup is inline because a top-level
-- splice cannot call a function defined in the same module.
nginzValues :: ByteString
nginzValues =
  $( do
       spliced <- makeRelativeToProject "test/unit/generated/nginz-values.yaml"
       spliced' <- runIO (doesFileExist spliced)
       embedFile
         =<< if spliced'
           then pure spliced
           else makeRelativeToProject "../../charts/nginz/values.yaml"
   )

instance A.FromJSON NginzLocations where
  parseJSON = A.withObject "charts/nginz/values.yaml" $ \top -> do
    conf <- top A..: "nginx_conf"
    ups <- conf A..: "upstreams"
    extra <- conf A..:? "extra_upstreams" A..!= Map.empty
    ignored <- conf A..:? "ignored_upstreams" A..!= []
    enabled <- conf A..:? "enabled_extra_upstreams" A..!= []
    pure
      . NginzLocations
      . concat
      . Map.elems
      $ Map.withoutKeys ups (Set.fromList (ignored :: [Text]))
        <> Map.restrictKeys extra (Set.fromList (enabled :: [Text]))

instance A.FromJSON Location where
  parseJSON = A.withObject "nginz upstream entry" $ \o -> do
    path <- o A..: "path"
    oldScope :: Maybe Text <- do
      s <- o A..:? "oauth_scope"
      forM s validateOldScope
    newScopes :: [Text] <- do
      s <- o A..:? "oauth_scopes" A..!= []
      forM s validateNewScope
    pure (Location path oldScope newScopes)

validateOldScope :: (MonadFail m) => Text -> m Text
validateOldScope s = if allowed then pure s else fail ("unknown old scope: " <> show s)
  where
    allowed =
      s
        `elem` [ "feature_configs",
                 "self",
                 "conversations",
                 "conversations_code",
                 "conversations_name",
                 "meetings"
               ]

-- | NB: this could be re-written in terms of `data OAuth{Scope,Tier}`
-- to auto-re-align it with changes, but at the time of writing, the
-- changes to the data type had not been implemented yet.
validateNewScope :: (MonadFail m) => Text -> m Text
validateNewScope s = if allowed then pure s else fail ("unknown new scope: " <> show s)
  where
    allowed = t && n
      where
        t =
          T.takeWhile (/= ':') s `elem` ["read", "write-only", "delete-only"]
        n =
          T.dropWhile (/= ':') s
            `elem` [ ":feature_configs",
                     ":self",
                     ":conversations",
                     ":conversations_code",
                     ":conversations_name",
                     ":meetings"
                   ]

pcreOnlyConstructs :: [Text]
pcreOnlyConstructs = ["(?", "\\", "{", "*?", "+?"]

--------------------------------------------------------------------------------
-- what the swagger docs claim

documentedScope :: Text -> Maybe Text
documentedScope descr = enc <$> find prop [minBound ..]
  where
    enc = T.decodeUtf8 . toByteString'
    prop = (`T.isInfixOf` descr) . renderOAuthScope

httpMethods :: [Text]
httpMethods = ["GET", "PUT", "POST", "DELETE", "OPTIONS", "HEAD", "PATCH", "TRACE"]

-- | @(path, method, description)@ for every operation in a swagger document.
operations :: A.Value -> [(Text, Text, Text)]
operations doc = do
  paths <- maybeToList (object doc >>= KeyMap.lookup "paths" >>= object)
  (path, pathItem) <- KeyMap.toList paths
  item <- maybeToList (object pathItem)
  (method, op) <- KeyMap.toList item
  let method' = T.toUpper (Key.toText method)
  guard (method' `elem` httpMethods)
  pure (Key.toText path, method', fromMaybe "" (object op >>= KeyMap.lookup "description" >>= string))
  where
    object = \case A.Object o -> Just o; _ -> Nothing
    string = \case A.String s -> Just s; _ -> Nothing

--------------------------------------------------------------------------------
-- the comparison

-- | 'Finding's are interesting iff @fEnforced /= fDocumented@.
data Finding = Finding
  { fVersion :: Version,
    fMethod :: Text,
    fPath :: Text,
    fEnforced :: Maybe Text,
    fDocumented :: Maybe Text
  }

renderFinding :: Finding -> Text
renderFinding f =
  T.intercalate
    "   "
    [ toUrlPiece (fVersion f),
      fMethod f,
      fPath f,
      T.pack . show . toList $ fEnforced f,
      T.pack . show . toList $ fDocumented f
    ]

findings :: [Finding]
findings =
  [ Finding devVersion method path enforced documented
  | (path, method, descr) <- operations (A.toJSON devVersionSwagger),
    let enforced = enforcedScope method path,
    let documented = documentedScope descr,
    enforced /= documented
  ]

--------------------------------------------------------------------------------
-- the actual tests

testPatternVocabulary :: Assertion
testPatternVocabulary =
  for_ nginzLocations $ \loc ->
    for_ pcreOnlyConstructs $ \bad ->
      when (bad `T.isInfixOf` locPattern loc) $
        assertFailure . T.unpack $
          "charts/nginz/values.yaml: the path pattern "
            <> locPattern loc
            <> " uses '"
            <> bad
            <> "', which nginx reads as PCRE but this test matches with regex-tdfa, "
            <> "i.e. POSIX ERE.  The two may disagree, which would be bad."

testScopesAgree :: Assertion
testScopesAgree = do
  unless (Set.null actual) . assertFailure . T.unpack . T.unlines $
    [ "OAuth scope declarations are out of sync.",
      "",
      "Columns: version, method, path, accepted by nginz, documented in swagger.",
      "'-' means no scope. A finding means those last two disagree:",
      "",
      "  enforced but not documented  charts/nginz/values.yaml requires a scope the",
      "                               swagger docs do not mention -- most likely a",
      "                               missing DescriptionOAuthScope in the routing",
      "                               table, e.g. on a newly added version of an",
      "                               endpoint that already had one.",
      "  documented but not enforced  the swagger docs promise a scope nginz does not",
      "                               require -- a stale annotation, or a missing",
      "                               oauth_scope: in charts/nginz/values.yaml.",
      ""
    ]
      <> section "deviations:" actual
  where
    actual = Set.fromList (renderFinding <$> findings)
    section title xs
      | Set.null xs = []
      | otherwise = ["  " <> title] <> (("    " <>) <$> Set.toAscList xs) <> [""]
