module Gundeck.Push.Data
  ( insert,
    delete,
    Gundeck.Push.Data.lookup,
    erase,
    Consistency (..),
  )
where

import Cassandra
import Data.ByteString.Conversion
import Data.Id (ClientId, ConnId, UserId)
import Gundeck.Instances ()
import Gundeck.Push.Native.Types
import Gundeck.Types
import Imports
import System.Logger.Class (MonadLogger, field, msg, val, (~~))
import qualified System.Logger.Class as Log

lookup :: (MonadClient m, MonadLogger m) => UserId -> Consistency -> m [Address]
lookup u c = foldM mk [] =<< retry x1 (query q (params c (Identity u)))
  where
    q :: PrepQuery R (Identity UserId) (UserId, Transport, AppName, Token, Maybe EndpointArn, ConnId, Maybe ClientId)
    q = "select usr, transport, app, ptoken, arn, connection, client from user_push where usr = ?"
    mk as r = maybe as (: as) <$> mkAddr r

insert :: MonadClient m => UserId -> Transport -> AppName -> Token -> EndpointArn -> ConnId -> ClientId -> m ()
insert u t a p e o c = retry x5 $ write q (params Quorum (u, t, a, p, e, o, c))
  where
    q :: PrepQuery W (UserId, Transport, AppName, Token, EndpointArn, ConnId, ClientId) ()
    q = "insert into user_push (usr, transport, app, ptoken, arn, connection, client) values (?, ?, ?, ?, ?, ?, ?)"

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

mkAddr ::
  (MonadClient m, MonadLogger m) =>
  (UserId, Transport, AppName, Token, Maybe EndpointArn, ConnId, Maybe ClientId) ->
  m (Maybe Address)
mkAddr (usr, trp, app, tok, arn, con, clt) = case (clt, arn) of
  (Just c, Just a) -> return $! Just $! Address usr a con (pushToken trp app tok c)
  _ -> do
    Log.info $
      field "user" (toByteString usr)
        ~~ field "transport" (show trp)
        ~~ field "app" (appNameText app)
        ~~ field "token" (tokenText tok)
        ~~ msg (val "Deleting legacy push token without a client or ARN.")
    delete usr trp app tok
    return Nothing
