-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Intra.User
  ( getConnections,
    deleteBot,
    reAuthUser,
    lookupActivatedUsers,
    getUser,
    getUsers,
    deleteUser,
    getContactList,
    chunkify,
    getBrigUserRichInfo,
  )
where

import Bilge hiding (getHeader, options, statusCode)
import Bilge.RPC
import Brig.Types.Connection (ConnectionsStatusRequest (..), Relation (..), UserIds (..))
import Brig.Types.Intra
import Brig.Types.User (User)
import Control.Monad.Catch (throwM)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Conversion
import Data.Id
import Galley.App
import Galley.Intra.Util
import Imports
import Network.HTTP.Client (HttpExceptionContent (..))
import qualified Network.HTTP.Client.Internal as Http
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error
import Wire.API.User.RichInfo (RichInfo)

-- | Get statuses of all connections between two groups of users (the usual
-- pattern is to check all connections from one user to several, or from
-- several users to one).
--
-- When a connection does not exist, it is skipped.
-- Calls 'Brig.API.getConnectionsStatusH'.
getConnections :: [UserId] -> [UserId] -> Maybe Relation -> Galley [ConnectionStatus]
getConnections uFrom uTo rlt = do
  (h, p) <- brigReq
  r <-
    call "brig" $
      method POST . host h . port p
        . path "/i/users/connections-status"
        . maybe id rfilter rlt
        . json ConnectionsStatusRequest {csrFrom = uFrom, csrTo = uTo}
        . expect2xx
  parseResponse (Error status502 "server-error") r
  where
    rfilter = queryItem "filter" . (pack . map toLower . show)

-- | Calls 'Brig.Provider.API.botGetSelfH'.
deleteBot :: ConvId -> BotId -> Galley ()
deleteBot cid bot = do
  (h, p) <- brigReq
  void $
    call "brig" $
      method DELETE . host h . port p
        . path "/bot/self"
        . header "Z-Type" "bot"
        . header "Z-Bot" (toByteString' bot)
        . header "Z-Conversation" (toByteString' cid)
        . expect2xx

-- | Calls 'Brig.User.API.Auth.reAuthUserH'.
reAuthUser :: UserId -> ReAuthUser -> Galley Bool
reAuthUser uid auth = do
  (h, p) <- brigReq
  let req =
        method GET . host h . port p
          . paths ["/i/users", toByteString' uid, "reauthenticate"]
          . json auth
  st <- statusCode . responseStatus <$> call "brig" (check [status200, status403] . req)
  return $ st == 200

check :: [Status] -> Request -> Request
check allowed r =
  r
    { Http.checkResponse = \rq rs ->
        when (responseStatus rs `notElem` allowed) $
          let ex = StatusCodeException (rs {responseBody = ()}) mempty
           in throwM $ HttpExceptionRequest rq ex
    }

-- | Calls 'Brig.API.listActivatedAccountsH'.
lookupActivatedUsers :: [UserId] -> Galley [User]
lookupActivatedUsers = chunkify $ \uids -> do
  (h, p) <- brigReq
  let users = BSC.intercalate "," $ toByteString' <$> uids
  r <-
    call "brig" $
      method GET . host h . port p
        . path "/i/users"
        . queryItem "ids" users
        . expect2xx
  parseResponse (Error status502 "server-error") r

-- | urls with more than 1k uids produce 400 response (url too long).  the exact limit (or how
-- stable it is) is unclear.  this is a conservative guess.
chunkify :: forall m key a. (Monad m, ToByteString key, Monoid a) => ([key] -> m a) -> [key] -> m a
chunkify doChunk keys = mconcat <$> (doChunk `mapM` chunks keys)
  where
    maxSize :: Int
    maxSize = 300

    chunks :: [any] -> [[any]]
    chunks [] = []
    chunks uids = case splitAt maxSize uids of (h, t) -> h : chunks t

-- | Calls 'Brig.API.listActivatedAccountsH'.
getUser :: UserId -> Galley (Maybe UserAccount)
getUser uid = listToMaybe <$> getUsers [uid]

-- | Calls 'Brig.API.listActivatedAccountsH'.
getUsers :: [UserId] -> Galley [UserAccount]
getUsers = chunkify $ \uids -> do
  (h, p) <- brigReq
  resp <-
    call "brig" $
      method GET . host h . port p
        . path "/i/users"
        . queryItem "ids" (BSC.intercalate "," (toByteString' <$> uids))
        . expect2xx
  pure . fromMaybe [] . responseJsonMaybe $ resp

-- | Calls 'Brig.API.deleteUserNoVerifyH'.
deleteUser :: UserId -> Galley ()
deleteUser uid = do
  (h, p) <- brigReq
  void $
    call "brig" $
      method DELETE . host h . port p
        . paths ["/i/users", toByteString' uid]
        . expect2xx

-- | Calls 'Brig.API.getContactListH'.
getContactList :: UserId -> Galley [UserId]
getContactList uid = do
  (h, p) <- brigReq
  r <-
    call "brig" $
      method GET . host h . port p
        . paths ["/i/users", toByteString' uid, "contacts"]
        . expect2xx
  cUsers <$> parseResponse (Error status502 "server-error") r

-- | Calls 'Brig.API.Internal.getRichInfoMultiH'
getRichInfoMultiUser :: [UserId] -> Galley [RichInfo]
getRichInfoMultiUser uids = do
  (h, p) <- brigReq
  resp <-
    call "brig" $
      method GET . host h . port p
        . paths ["/i/users/rich-info"]
        . param ""
        . expect2xx
  parseResponse (Error status502 "server-error") resp
