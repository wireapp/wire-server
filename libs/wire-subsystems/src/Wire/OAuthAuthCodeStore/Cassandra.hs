-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Cassandra interpreter for the OAuth authorization-code store. The queries
-- are moved verbatim from the former inline CQL in @Brig.API.OAuth@.
module Wire.OAuthAuthCodeStore.Cassandra
  ( oauthAuthCodeStoreToCassandra,
  )
where

import Cassandra
import Data.Id (OAuthClientId, UserId)
import Imports
import Polysemy
import Wire.API.OAuth
import Wire.OAuthAuthCodeStore

oauthAuthCodeStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (OAuthAuthCodeStore ': r) a ->
  Sem r a
oauthAuthCodeStoreToCassandra =
  interpret $
    embed @m
      . \case
        InsertOAuthAuthCode ttl code cid uid scope uri chal ->
          retry x5
            . write insertQuery
            . params LocalQuorum
            $ (code, cid, uid, scope, uri, chal, ttl)
        LookupAndDeleteOAuthAuthCode code -> lookupAndDelete code
  where
    insertQuery :: PrepQuery W (OAuthAuthorizationCode, OAuthClientId, UserId, OAuthScopes, RedirectUrl, OAuthCodeChallenge, Int32) ()
    insertQuery =
      "INSERT INTO oauth_auth_code (code, client, user, scope, redirect_uri, code_challenge) VALUES (?, ?, ?, ?, ?, ?) USING TTL ?"

    lookupAndDelete :: OAuthAuthorizationCode -> m (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge))
    lookupAndDelete code = lookupOAuthAuthorizationCode <* deleteOAuthAuthorizationCode
      where
        lookupOAuthAuthorizationCode :: m (Maybe (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge))
        lookupOAuthAuthorizationCode = do
          retry x5 . query1 q $ params LocalQuorum (Identity code)
          where
            q :: PrepQuery R (Identity OAuthAuthorizationCode) (OAuthClientId, UserId, OAuthScopes, RedirectUrl, Maybe OAuthCodeChallenge)
            q = "SELECT client, user, scope, redirect_uri, code_challenge FROM oauth_auth_code WHERE code = ?"

        deleteOAuthAuthorizationCode :: m ()
        deleteOAuthAuthorizationCode = retry x5 . write q $ params LocalQuorum (Identity code)
          where
            q :: PrepQuery W (Identity OAuthAuthorizationCode) ()
            q = "DELETE FROM oauth_auth_code WHERE code = ?"
