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

module Gundeck.Push.Data
  ( insert,
    updateArn,
    delete,
    lookup,
    erase,
    Consistency (..),
  )
where

import Cassandra
import Data.ByteString.Conversion
import Data.Id (ClientId, ConnId, UserId)
import Gundeck.Instances ()
import Gundeck.Push.Native.Types
import Gundeck.Types hiding (token)
import Imports hiding (lookup)
import System.Logger.Class (MonadLogger, field, msg, val, (~~))
import System.Logger.Class qualified as Log

lookup :: (MonadClient m, MonadLogger m) => UserId -> Consistency -> m [Address]
lookup u c = foldM mk [] =<< retry x1 (query q (params c (Identity u)))
  where
    q :: PrepQuery R (Identity UserId) (UserId, Transport, AppName, Token, Maybe EndpointArn, ConnId, Maybe ClientId)
    q = "select usr, transport, app, ptoken, arn, connection, client from user_push where usr = ?"
    mk as r = maybe as (: as) <$> mkAddr r

insert :: (MonadClient m) => UserId -> Transport -> AppName -> Token -> EndpointArn -> ConnId -> ClientId -> m ()
insert u t a p e o c = retry x5 $ write q (params LocalQuorum (u, t, a, p, e, o, c))
  where
    q :: PrepQuery W (UserId, Transport, AppName, Token, EndpointArn, ConnId, ClientId) ()
    q = "insert into user_push (usr, transport, app, ptoken, arn, connection, client) values (?, ?, ?, ?, ?, ?, ?)"

updateArn :: (MonadClient m) => UserId -> Transport -> AppName -> Token -> EndpointArn -> m ()
updateArn uid transport app token arn = retry x5 $ write q (params LocalQuorum (arn, uid, transport, app, token))
  where
    q :: PrepQuery W (EndpointArn, UserId, Transport, AppName, Token) ()
    q = {- `IF EXISTS`, but that requires benchmarking -} "update user_push set arn = ? where usr = ? and transport = ? and app = ? and ptoken = ?"

delete :: (MonadClient m) => UserId -> Transport -> AppName -> Token -> m ()
delete u t a p = retry x5 $ write q (params LocalQuorum (u, t, a, p))
  where
    q :: PrepQuery W (UserId, Transport, AppName, Token) ()
    q = "delete from user_push where usr = ? and transport = ? and app = ? and ptoken = ?"

erase :: (MonadClient m) => UserId -> m ()
erase u = retry x5 $ write q (params LocalQuorum (Identity u))
  where
    q :: PrepQuery W (Identity UserId) ()
    q = "delete from user_push where usr = ?"

mkAddr ::
  (MonadClient m, MonadLogger m) =>
  (UserId, Transport, AppName, Token, Maybe EndpointArn, ConnId, Maybe ClientId) ->
  m (Maybe Address)
mkAddr (usr, trp, app, tok, arn, con, clt) = case (clt, arn) of
  (Just c, Just a) -> pure $! Just $! Address usr a con (pushToken trp app tok c)
  _ -> do
    Log.info $
      field "user" (toByteString usr)
        ~~ field "transport" (show trp)
        ~~ field "app" (appNameText app)
        ~~ field "token" (tokenText tok)
        ~~ msg (val "Deleting legacy push token without a client or ARN.")
    delete usr trp app tok
    pure Nothing
