{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Push.Data
    ( insert
    , delete
    , Gundeck.Push.Data.lookup
    , erase
    , Consistency (..)
    ) where

import Imports
import Cassandra
import Data.ByteString.Conversion
import Data.Id (UserId, ClientId, ConnId)
import Gundeck.Instances ()
import Gundeck.Push.Native.Types
import Gundeck.Types
import System.Logger.Class (MonadLogger, val, msg, field, (~~))

import qualified System.Logger.Class as Log

lookup :: (MonadClient m, MonadLogger m) => UserId -> Consistency -> m [Address "no-keys"]
lookup u c = foldM mk [] =<< retry x1 (query q (params c (Identity u)))
  where
    q :: PrepQuery R (Identity UserId) (UserId, Transport, AppName, Token, Maybe EndpointArn, ConnId, Maybe ClientId, Maybe Transport)
    q = "select usr, transport, app, ptoken, arn, connection, client, fallback from user_push where usr = ?"

    mk as r = maybe as (:as) <$> mkAddr r

insert :: MonadClient m => UserId -> Transport -> AppName -> Token -> EndpointArn -> ConnId -> ClientId -> Maybe Transport -> m ()
insert u t a p e o c f = retry x5 $ write q (params Quorum (u, t, a, p, e, o, c, f))
  where
    q :: PrepQuery W (UserId, Transport, AppName, Token, EndpointArn, ConnId, ClientId, Maybe Transport) ()
    q = "insert into user_push (usr, transport, app, ptoken, arn, connection, client, fallback) values (?, ?, ?, ?, ?, ?, ?, ?)"

delete :: MonadClient m => UserId -> Transport -> AppName -> Token -> m ()
delete u t a p = retry x5 $ write q (params Quorum (u, t, a, p))
  where
    q :: PrepQuery W (UserId, Transport, AppName, Token) ()
    q = "delete from user_push where usr = ? and transport = ? and app = ? and ptoken = ?"

erase :: MonadClient m => UserId -> m ()
erase u = retry x5 $ write q (params Quorum (Identity u))
  where
    q :: PrepQuery W (Identity UserId) ()
    q = "delete from user_push where usr = ?"

mkAddr :: (MonadClient m, MonadLogger m)
       => (UserId, Transport, AppName, Token, Maybe EndpointArn, ConnId, Maybe ClientId, Maybe Transport)
       -> m (Maybe (Address "no-keys"))
mkAddr (usr, trp, app, tok, arn, con, clt, fbt) = case (clt, arn) of
    (Just c, Just a) -> return $! Just $! Address usr trp app tok a con c Nothing fbt
    _                -> do
        Log.info $ field "user" (toByteString usr)
            ~~ field "transport" (show trp)
            ~~ field "app" (appNameText app)
            ~~ field "token" (tokenText tok)
            ~~ msg (val "Deleting legacy push token without a client or ARN.")
        delete usr trp app tok
        return Nothing

