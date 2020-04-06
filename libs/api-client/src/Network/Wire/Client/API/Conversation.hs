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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Wire.Client.API.Conversation
  ( postOtrMessage,
    createConv,
    getConv,
    addMembers,
    removeMember,
    memberUpdate,
    module M,
  )
where

import Bilge
import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Conversion
import Data.Id
import Data.List.NonEmpty hiding (cons, toList)
import Data.List1
import Data.Text (pack)
import Galley.Types as M hiding (Event, EventType, memberUpdate)
import Galley.Types.Conversations.Roles (roleNameWireAdmin)
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status hiding (statusCode)
import Network.Wire.Client.API.Push (ConvEvent)
import Network.Wire.Client.HTTP
import Network.Wire.Client.Monad (ClientException (ParseError))
import Network.Wire.Client.Session

postOtrMessage :: MonadSession m => ConvId -> NewOtrMessage -> m ClientMismatch
postOtrMessage cnv msg = sessionRequest req rsc readBody
  where
    req =
      method POST
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
addMembers :: (MonadSession m, MonadThrow m) => ConvId -> List1 UserId -> m (Maybe (ConvEvent SimpleMembers))
addMembers cnv (fmap makeIdOpaque -> mems) = do
  rs <- sessionRequest req rsc consumeBody
  case statusCode rs of
    200 -> Just <$> responseJsonThrow (ParseError . pack) rs
    204 -> return Nothing
    _ -> unexpected rs "addMembers: status code"
  where
    req =
      method POST
        . paths ["conversations", toByteString' cnv, "members"]
        . acceptJson
        . json (newInvite mems)
        $ empty
    rsc = status200 :| [status204]

-- | Remove a user and (in case of success) return the event corresponding
-- to the user removal.
removeMember :: (MonadSession m, MonadThrow m) => ConvId -> UserId -> m (Maybe (ConvEvent UserIdList))
removeMember cnv mem = do
  rs <- sessionRequest req rsc consumeBody
  case statusCode rs of
    200 -> Just <$> responseJsonThrow (ParseError . pack) rs
    204 -> return Nothing
    _ -> unexpected rs "removeMember: status code"
  where
    req =
      method DELETE
        . paths ["conversations", toByteString' cnv, "members", toByteString' mem]
        . acceptJson
        $ empty
    rsc = status200 :| [status204]

memberUpdate :: MonadSession m => ConvId -> MemberUpdateData -> m ()
memberUpdate cnv updt = sessionRequest req rsc (const $ return ())
  where
    req =
      method PUT
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
    _ -> unexpected rs "getConv: status code"
  where
    req =
      method GET
        . paths ["conversations", toByteString' cnv]
        . acceptJson
        $ empty
    rsc = status200 :| [status404]

-- | Create a conversation with the session user in it and any number of
-- other users (possibly zero).
createConv ::
  MonadSession m =>
  -- | Other users to add to the conversation
  [UserId] ->
  -- | Conversation name
  Maybe Text ->
  m Conversation
createConv (fmap makeIdOpaque -> users) name = sessionRequest req rsc readBody
  where
    req =
      method POST
        . path "conversations"
        . acceptJson
        . json (NewConvUnmanaged (NewConv users name mempty Nothing Nothing Nothing Nothing roleNameWireAdmin))
        $ empty
    rsc = status201 :| []
