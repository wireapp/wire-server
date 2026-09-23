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

module Test.Wire.API.Routes.OAuthScopes (tests) where

-- TODO: test backwards compatibility (copy old values.yaml to wire-api tests and run them against the same swagger.

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Conversion
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
import Wire.API.OAuth
import Wire.API.Routes.Public (renderOAuthScope)
import Wire.API.Routes.Public.Swagger (devVersion, devVersionSwagger)
import Wire.API.Routes.Version

tests :: TestTree
tests =
  testGroup
    "OAuth scopes (charts/nginz/values.yaml vs. swagger docs)"
    [ testCase "enforced scopes and documented scopes agree" testScopesAgree,
      testCase "nginz path patterns avoid PCRE-only constructs" testPatternVocabulary
    ]

-- | Two independent places declare which OAuth scope an endpoint needs, and
-- nothing keeps them in sync:
--
-- 1. @charts/nginz/values.yaml@: @oauth_scope:@ (deprecated) or @oauth_scopes:@
--    on an upstream entry.  This is what is actually /enforced/: nginz rejects
--    OAuth tokens without a matching scope, and rejects all of them
--    where no scope is configured at all.
-- 2. The servant routing tables: 'Wire.API.Routes.Public.DescriptionOAuthScope'.
--    This is only documentation: it appends a line to the endpoint description
--    in the swagger docs and has no effect on request handling.
--
-- This test matches the openapi docs generated from servant routes
-- against what nginz enforces.  The behavior of nginz is emulated by
-- this test.  Actual behavior of libzauth is tested in the rust code;
-- those tests and these here need to be kept in sync manually
-- (compare `enforcedScopes` below with
-- `/libs/libzauth/libzauth/src/oauth.rs` (search for `mod tests`)).
testScopesAgree :: Assertion
testScopesAgree = do
  unless (Set.null actual) . assertFailure . T.unpack . T.unlines $
    [ "OAuth scope declarations are out of sync.",
      "",
      "Columns: version, method, path, accepted by nginz, documented in swagger.",
      "The nginz column lists every scope that gets an OAuth token through to that",
      "verb; '[]' means none does, i.e. OAuth is not usable there at all (the route",
      "may still be reachable with a zauth cookie or token).  A finding means",
      "swagger.json does not match values.yaml:",
      "",
      "  accepted but not documented  charts/nginz/values.yaml lets a scope through",
      "                               that the swagger docs do not mention -- most",
      "                               likely a missing DescriptionOAuthScope in the",
      "                               routing table, e.g. on a newly added version of",
      "                               an endpoint that already had one.",
      "  documented but not accepted  the swagger docs promise a scope that does not",
      "                               get anybody in -- a stale annotation, or a scope",
      "                               missing from charts/nginz/values.yaml.",
      ""
    ]
      <> section "deviations:" actual
  where
    actual = Set.fromList (renderFinding <$> findings)
    section title xs
      | Set.null xs = []
      | otherwise = ["  " <> title] <> (("    " <>) <$> Set.toAscList xs) <> [""]

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

--------------------------------------------------------------------------------
-- what nginz enforces

data OAuthTier = Read | WriteOnly | DeleteOnly
  deriving (Eq, Show)

instance ToByteString OAuthTier where
  builder = \case
    Read -> "read"
    WriteOnly -> "write-only"
    DeleteOnly -> "delete-only"

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
    locOldScope :: Maybe Text, -- only the base, e.g. "conversations_code": no tier without the verb.
    locNewScopes :: Maybe [OAuthScope]
  }

-- | Which scopes let an OAuth token through to this method and path?  The
-- answer is always given in new scopes, also where values.yaml still uses old
-- ones.
--
-- If the matching location has @oauth_scopes@, the answer is those of the
-- listed scopes that have the tier this verb needs.
--
-- TODO: the following paragraph is less than clear, rephrase!
--
-- If it only has the deprecated @oauth_scope@, the answer is the one scope made
-- of that base and the tier this verb needs: under @oauth_scope:
-- conversations_code@, a @GET@ wants @read:conversations_code@ and nothing
-- else.  Old scopes are cumulative, so a token carrying
-- @write:conversations_code@ passes that @GET@ as well, but nginz reads that off
-- the token rather than off the configuration (@granted_scopes@ in
-- @libs/libzauth/libzauth/src/oauth.rs@), and it does not change which scope the
-- docs should name.
--
-- NB: an empty answer means no OAuth token gets in at all.  That happens if
-- the location has no @oauth_scope[s]@, if its @oauth_scopes@ list has nothing
-- of the tier the verb needs, or if the verb is one nginz has no rule for.
enforcedScopes :: Text -> Text -> Set OAuthScope
enforcedScopes method path = case find locationMatches nginzLocations of
  Nothing -> Set.empty
  Just loc -> case (loc.locOldScope, loc.locNewScopes) of
    (_, Just newScopes) ->
      -- Filter scopes listed in values.yaml by matching method/tier.
      Set.fromList (filter (hasTierFor method newScopes)
    (Just base, Nothing) ->
      -- The deprecated attribute gives the base; the tier comes from the method.
      maybe Set.empty Set.singleton (oldScopeBase base)
    (Nothing, Nothing) -> Set.empty
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

    hasTierFor :: Text -> OAuthScope -> Bool
    hasTierFor method scope = case newTier method of
      Nothing _ -> False
      Just tier ->      T.decodeUtf8 (toByteString' tier <> ":")
        `T.isPrefixOf` T.decodeUtf8 (toByteString' scope)

    newTier :: Text -> Maybe OAuthTier
    newTier = \case
      "GET" -> Just Read
      "POST" -> Just WriteOnly
      "PUT" -> Just WriteOnly
      "DELETE" -> Just DeleteOnly
      _ -> Nothing

    -- Mirrors @verify_scope@ in @libs/libzauth/libzauth/src/oauth.rs@, which is
    -- what nginz calls for a location with the deprecated attribute.  'Nothing'
    -- for a base that is no scope of ours, e.g. a typo in values.yaml.
    oldScopeBase :: Text -> Maybe OAuthScope
    oldScopeBase base = do
      tier <- newTier method
      fromByteString (toByteString' tier <> ":" <> T.encodeUtf8 base)

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
      o A..:? "oauth_scope"
    newScopes :: Maybe [OAuthScope] <- do
      mbs :: Maybe [Text] <- o A..:? "oauth_scopes"
      mapM (mapM validateNewScope) mbs
    pure (Location path oldScope newScopes)

validateNewScope :: (MonadFail m) => Text -> m OAuthScope
validateNewScope s =
  fromByteString @OAuthScope (T.encodeUtf8 s)
    & maybe
      (fail ("unknown new scope: " <> show s))
      pure

pcreOnlyConstructs :: [Text]
pcreOnlyConstructs = ["(?", "\\", "{", "*?", "+?"]

--------------------------------------------------------------------------------
-- what the swagger docs claim

documentedScope :: Text -> Maybe OAuthScope
documentedScope descr = find ((`T.isInfixOf` descr) . renderOAuthScope) [minBound ..]

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

-- | 'Finding's are interesting iff 'fDocumented' is not one of 'fEnforced'.
data Finding = Finding
  { fVersion :: Version,
    fMethod :: Text,
    fPath :: Text,
    fEnforced :: Set OAuthScope,
    fDocumented :: Maybe OAuthScope
  }

renderFinding :: Finding -> Text
renderFinding f =
  T.intercalate
    "   "
    [ toUrlPiece (fVersion f),
      fMethod f,
      fPath f,
      T.pack . show . Set.toList $ fEnforced f,
      T.pack . show . toList $ fDocumented f
    ]

findings :: [Finding]
findings =
  [ Finding devVersion method path enforced documented
  | (path, method, descr) <- operations (A.toJSON devVersionSwagger),
    let enforced = enforcedScopes method path,
    let documented = documentedScope descr,
    not (scopesMatch enforced documented)
  ]

-- | The documented scope has to be one of the scopes that actually get a token
-- through.  If no token gets through at all, there is nothing to document.
scopesMatch ::
  -- | required
  Set OAuthScope ->
  -- | documented
  Maybe OAuthScope ->
  Bool
scopesMatch enforced = maybe (Set.null enforced) (`Set.member` enforced)
