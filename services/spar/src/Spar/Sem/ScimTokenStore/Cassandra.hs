{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}


-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Spar.Sem.ScimTokenStore.Cassandra
  ( scimTokenStoreToCassandra,
  )
where

import Cassandra as Cas
import Control.Arrow (Arrow ((&&&)))
import Control.Lens
import Control.Monad.Except
import Data.Id
import Data.Time
import Imports
import Polysemy
import qualified SAML2.WebSSO as SAML
import Spar.Data.Instances ()
import Spar.Sem.ScimTokenStore
import Text.RawString.QQ
import Wire.API.User.Scim
import qualified Prelude

scimTokenStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (ScimTokenStore ': r) a ->
  Sem r a
scimTokenStoreToCassandra =
  interpret $
    embed @m . \case
      Insert st sti -> insertScimToken st sti
      Lookup st -> lookupScimToken st
      LookupByTeam tid -> getScimTokens tid
      Delete tid ur -> deleteScimToken tid ur
      DeleteByTeam tid -> deleteTeamScimTokens tid

----------------------------------------------------------------------
-- SCIM auth
--
-- docs/developer/scim/storage.md {#DevScimStorageTokens}

-- | Add a new SCIM provisioning token. The token should be random and
-- generated by the backend, not by the user.
insertScimToken ::
  (HasCallStack, MonadClient m) =>
  ScimToken ->
  ScimTokenInfo ->
  m ()
insertScimToken token ScimTokenInfo {..} = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  let tokenHash = hashScimToken token
  addPrepQuery insByToken (ScimTokenLookupKeyHashed tokenHash, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr)
  addPrepQuery insByTeam (ScimTokenLookupKeyHashed tokenHash, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr)

insByToken, insByTeam :: PrepQuery W ScimTokenRow ()
insByToken =
  [r|
    INSERT INTO team_provisioning_by_token
      (token_, team, id, created_at, idp, descr)
      VALUES (?, ?, ?, ?, ?, ?)
  |]
insByTeam =
  [r|
    INSERT INTO team_provisioning_by_team
      (token_, team, id, created_at, idp, descr)
      VALUES (?, ?, ?, ?, ?, ?)
  |]

scimTokenLookupKey :: ScimTokenRow -> ScimTokenLookupKey
scimTokenLookupKey (key, _, _, _, _, _) = key

-- | Check whether a token exists and if yes, what team and IdP are
-- associated with it.
lookupScimToken ::
  (HasCallStack, MonadClient m) =>
  ScimToken ->
  m (Maybe ScimTokenInfo)
lookupScimToken token = do
  let tokenHash = hashScimToken token
  rows <- retry x1 . query sel $ params LocalQuorum (tokenHash, token)
  case fmap (scimTokenLookupKey &&& Prelude.id) rows of
    [(ScimTokenLookupKeyHashed _, row)] ->
      pure (Just (fromScimTokenRow row))
    [(ScimTokenLookupKeyPlaintext plain, row)] ->
      convert plain row
    [(ScimTokenLookupKeyHashed _, _), (ScimTokenLookupKeyPlaintext plain, row)] ->
      convert plain row
    [(ScimTokenLookupKeyPlaintext plain, row), (ScimTokenLookupKeyHashed _, _)] ->
      convert plain row
    _ -> pure Nothing
  where
    sel :: PrepQuery R (ScimTokenHash, ScimToken) ScimTokenRow
    sel =
      [r|
      SELECT token_, team, id, created_at, idp, descr
        FROM team_provisioning_by_token WHERE token_ in (?, ?)
      |]

    convert :: MonadClient m => ScimToken -> ScimTokenRow -> m (Maybe ScimTokenInfo)
    convert plain row = do
      let tokenInfo = fromScimTokenRow row
      connvertPlaintextToken plain tokenInfo
      pure (Just tokenInfo)

connvertPlaintextToken ::
  (HasCallStack, MonadClient m) =>
  ScimToken ->
  ScimTokenInfo ->
  m ()
connvertPlaintextToken token ScimTokenInfo {..} = retry x5 . batch $ do
  setType BatchLogged
  setConsistency LocalQuorum
  let tokenHash = hashScimToken token
  -- enter by new lookup key
  addPrepQuery insByToken (ScimTokenLookupKeyHashed tokenHash, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr)
  -- update info table
  addPrepQuery insByTeam (ScimTokenLookupKeyHashed tokenHash, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr)
  -- remove old lookup key
  addPrepQuery delByTokenLookup (Identity (ScimTokenLookupKeyPlaintext token))

-- | List all tokens associated with a team, in the order of their creation.
getScimTokens ::
  (HasCallStack, MonadClient m) =>
  TeamId ->
  m [ScimTokenInfo]
getScimTokens team = do
  -- We don't need pagination here because the limit should be pretty low
  -- (e.g. 16). If the limit grows, we might have to introduce pagination.
  rows <- retry x1 . query sel $ params LocalQuorum (Identity team)
  pure $ sortOn stiCreatedAt $ map fromScimTokenRow rows
  where
    sel :: PrepQuery R (Identity TeamId) ScimTokenRow
    sel =
      [r|
      SELECT token_, team, id, created_at, idp, descr
        FROM team_provisioning_by_team WHERE team = ?
      |]

-- | Delete a token.
deleteScimToken ::
  (HasCallStack, MonadClient m) =>
  TeamId ->
  ScimTokenId ->
  m ()
deleteScimToken team tokenid = do
  mbToken <- retry x1 . query1 selById $ params LocalQuorum (team, tokenid)
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery delById (team, tokenid)
    for_ mbToken $ \(Identity key) ->
      addPrepQuery delByTokenLookup (Identity key)
  where
    selById :: PrepQuery R (TeamId, ScimTokenId) (Identity ScimTokenLookupKey)
    selById =
      [r|
      SELECT token_ FROM team_provisioning_by_team
        WHERE team = ? AND id = ?
    |]

delById :: PrepQuery W (TeamId, ScimTokenId) ()
delById =
  [r|
  DELETE FROM team_provisioning_by_team
    WHERE team = ? AND id = ?
  |]

delByTokenLookup :: PrepQuery W (Identity ScimTokenLookupKey) ()
delByTokenLookup =
  [r|
  DELETE FROM team_provisioning_by_token
    WHERE token_ = ?
|]

-- | Delete all tokens belonging to a team.
deleteTeamScimTokens ::
  (HasCallStack, MonadClient m) =>
  TeamId ->
  m ()
deleteTeamScimTokens team = do
  tokens <- retry x5 $ query sel $ params LocalQuorum (Identity team)
  retry x5 . batch $ do
    setType BatchLogged
    setConsistency LocalQuorum
    addPrepQuery delByTeam (Identity team)
    mapM_ (addPrepQuery delByTokenLookup) tokens
  where
    sel :: PrepQuery R (Identity TeamId) (Identity ScimTokenLookupKey)
    sel = "SELECT token_ FROM team_provisioning_by_team WHERE team = ?"
    delByTeam :: PrepQuery W (Identity TeamId) ()
    delByTeam = "DELETE FROM team_provisioning_by_team WHERE team = ?"

type ScimTokenRow = (ScimTokenLookupKey, TeamId, ScimTokenId, UTCTime, Maybe SAML.IdPId, Text)

fromScimTokenRow :: ScimTokenRow -> ScimTokenInfo
fromScimTokenRow (_, stiTeam, stiId, stiCreatedAt, stiIdP, stiDescr) =
  ScimTokenInfo {..}
