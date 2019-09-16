{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Network.Wire.Client.API.Conversation
    ( postOtrMessage
    , createConv
    , getConv
    , addMembers
    , removeMember
    , memberUpdate
    , module M
    ) where

import Imports
import Bilge
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty hiding (cons, toList)
import Data.List1
import Data.Text (pack)
import Galley.Types as M hiding (Event, EventType)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Session
import Network.Wire.Client.API.Push (ConvEvent)
import Network.Wire.Client.Monad (ClientException(ParseError))

postOtrMessage :: MonadSession m => ConvId -> NewOtrMessage -> m ClientMismatch
postOtrMessage cnv msg = sessionRequest req rsc readBody
  where
    req = method POST
        . paths ["conversations", toByteString' cnv, "otr", "messages"]
        . acceptJson
        . json msg
        $ empty
    rsc = status201 :| [status412]

-- | Add one or more users and (in case of success) return the event
-- corresponding to the users addition.
--
-- If some users can not be added to the conversation, 'UnexpectedResponse'
-- will be thrown. It's not possible that some users will be added and
-- others will not.
addMembers :: (MonadSession m, MonadThrow m) => ConvId -> List1 UserId -> m (Maybe (ConvEvent Members))
addMembers cnv mems = do
    rs <- sessionRequest req rsc consumeBody
    case statusCode rs of
        200 -> Just <$> responseJsonThrow (ParseError . pack) rs
        204 -> return Nothing
        _   -> unexpected rs "addMembers: status code"
  where
    req = method POST
        . paths ["conversations", toByteString' cnv, "members"]
        . acceptJson
        . json (Invite mems)
        $ empty
    rsc = status200 :| [status204]

-- | Remove a user and (in case of success) return the event corresponding
-- to the user removal.
removeMember :: (MonadSession m, MonadThrow m) => ConvId -> UserId -> m (Maybe (ConvEvent Members))
removeMember cnv mem = do
    rs <- sessionRequest req rsc consumeBody
    case statusCode rs of
        200 -> Just <$> responseJsonThrow (ParseError . pack) rs
        204 -> return Nothing
        _   -> unexpected rs "removeMember: status code"
  where
    req = method DELETE
        . paths ["conversations", toByteString' cnv, "members", toByteString' mem]
        . acceptJson
        $ empty
    rsc = status200 :| [status204]

memberUpdate :: MonadSession m => ConvId -> MemberUpdateData -> m ()
memberUpdate cnv updt = sessionRequest req rsc (const $ return ())
  where
    req = method PUT
        . paths ["conversations", toByteString' cnv, "self"]
        . acceptJson
        . json updt
        $ empty
    rsc = status200 :| []

getConv :: (MonadSession m, MonadThrow m) => ConvId -> m (Maybe Conversation)
getConv cnv = do
    rs <- sessionRequest req rsc consumeBody
    case statusCode rs of
        200 -> responseJsonThrow (ParseError . pack) rs
        404 -> return Nothing
        _   -> unexpected rs "getConv: status code"
  where
    req = method GET
        . paths ["conversations", toByteString' cnv]
        . acceptJson
        $ empty
    rsc = status200 :| [status404]

-- | Create a conversation with the session user in it and any number of
-- other users (possibly zero).
createConv :: MonadSession m
           => [UserId]            -- ^ Other users to add to the conversation
           -> Maybe Text          -- ^ Conversation name
           -> m Conversation
createConv users name = sessionRequest req rsc readBody
  where
    req = method POST
        . path "conversations"
        . acceptJson
        . json (NewConvUnmanaged (NewConv users name mempty Nothing Nothing Nothing Nothing))
        $ empty
    rsc = status201 :| []
