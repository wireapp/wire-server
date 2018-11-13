{-# LANGUAGE OverloadedStrings #-}

module Galley.Intra.User
    ( getConnections
    , deleteBot
    , reAuthUser
    , lookupActivatedUsers
    , deleteUser
    , getContactList
    , canBeDeleted
    ) where

import Imports
import Bilge hiding (options, getHeader, statusCode)
import Brig.Types.Connection (UserIds (..))
import Bilge.RPC
import Brig.Types.Intra (ConnectionStatus (..), ReAuthUser (..))
import Brig.Types.Connection (Relation (..), ConnectionsStatusRequest (..))
import Brig.Types.User (User)
import Galley.App
import Galley.Intra.Util
import Galley.Types.Teams
import Control.Lens
import Control.Monad.Catch (throwM)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.Id
import Network.HTTP.Client (HttpException (..), HttpExceptionContent (..))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

import qualified Data.ByteString.Char8        as BSC
import qualified Network.HTTP.Client.Internal as Http

-- | Get statuses of all connections between two groups of users (the usual
-- pattern is to check all connections from one user to several, or from
-- several users to one).
--
-- When a connection does not exist, it is skipped.
getConnections :: [UserId] -> [UserId] -> Maybe Relation -> Galley [ConnectionStatus]
getConnections uFrom uTo rlt = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method POST . host h . port p
        . path "/i/users/connections-status"
        . maybe id rfilter rlt
        . json ConnectionsStatusRequest{csrFrom = uFrom, csrTo = uTo}
        . expect2xx
    parseResponse (Error status502 "server-error") r
  where
    rfilter = queryItem "filter" . (pack . map toLower . show)

deleteBot :: ConvId -> BotId -> Galley ()
deleteBot cid bot = do
    (h, p) <- brigReq
    void $ call "brig"
        $ method DELETE . host h . port p
        . path "/bot/self"
        . header "Z-Type" "bot"
        . header "Z-Bot" (toByteString' bot)
        . header "Z-Conversation" (toByteString' cid)
        . expect2xx

reAuthUser :: UserId -> ReAuthUser -> Galley Bool
reAuthUser uid auth = do
    (h, p) <- brigReq
    let req = method GET . host h . port p
            . paths ["/i/users", toByteString' uid, "reauthenticate"]
            . json auth
    st <- statusCode . responseStatus <$> call "brig" (check [status200, status403] . req)
    return $ st == 200

check :: [Status] -> Request -> Request
check allowed r = r { Http.checkResponse = \rq rs ->
    when (responseStatus rs `notElem` allowed) $
        let ex = StatusCodeException (rs { responseBody = () }) mempty
        in throwM $ HttpExceptionRequest rq ex
}

lookupActivatedUsers :: [UserId] -> Galley [User]
lookupActivatedUsers uids = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method GET . host h . port p
        . path "/i/users"
        . queryItem "ids" users
        . expect2xx
    parseResponse (Error status502 "server-error") r
  where
    users = BSC.intercalate "," $ toByteString' <$> uids

deleteUser :: UserId -> Galley ()
deleteUser uid = do
    (h, p) <- brigReq
    void $ call "brig"
        $ method DELETE . host h . port p
        . paths ["/i/users", toByteString' uid]
        . expect2xx

getContactList :: UserId -> Galley [UserId]
getContactList uid = do
    (h, p) <- brigReq
    r <- call "brig"
        $ method GET . host h . port p
        . paths ["/i/users", toByteString' uid, "contacts"]
        . expect2xx
    cUsers <$> parseResponse (Error status502 "server-error") r

canBeDeleted :: [TeamMember] -> UserId -> TeamId -> Galley Bool
canBeDeleted members uid tid = if askGalley then pure True else askBrig
  where
    -- team members without full permissions can always be deleted.
    askGalley = case filter ((== uid) . (^. userId)) members of
        (mem:_) -> not (isTeamOwner mem)
        _ -> False  -- e.g., if caller has no members and passes an empty list.

    -- only if still in doubt, ask brig.
    askBrig = do
        (h, p) <- brigReq
        st <- statusCode . responseStatus <$> call "brig"
            ( check [status200, status403]
            . method GET . host h . port p
            . paths ["/i/users", toByteString' uid, "can-be-deleted", toByteString' tid]
            )
        return $ st == 200
