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

module API
  ( createChannel,
    associateChannelsToGroup,
  )
where

import Control.Exception (try)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Range
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Imports
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Types
import Wire.API.Conversation
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.User (BaseProtocolTag (..))
import Wire.API.UserGroup (UpdateUserGroupChannels (..))

createChannel ::
  Manager ->
  ApiUrl ->
  Token ->
  UserId ->
  TeamId ->
  ChannelName ->
  IO (Either ErrorDetail ConvId)
createChannel manager (ApiUrl apiUrl) (Token token) userId teamId channelName = do
  let url = apiUrl <> "/v12/conversations"
      newConv =
        NewConv
          { newConvUsers = [],
            newConvQualifiedUsers = [],
            newConvName = Just (unsafeRange (fromRange (fromChannelName channelName))),
            newConvAccess = Set.empty,
            newConvAccessRoles = Just (Set.singleton TeamMemberAccessRole),
            newConvTeam = Just (ConvTeamInfo teamId),
            newConvMessageTimer = Nothing,
            newConvReceiptMode = Nothing,
            newConvUsersRole = roleNameWireAdmin,
            newConvProtocol = BaseProtocolMLSTag,
            newConvGroupConvType = Channel,
            newConvCells = False,
            newConvChannelAddPermission = Nothing,
            newConvSkipCreator = False,
            newConvParent = Nothing
          }
      body = encode newConv

  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST",
            requestHeaders =
              [ ("Authorization", "Bearer " <> Text.encodeUtf8 token),
                ("Content-Type", "application/json"),
                ("Z-User", Text.encodeUtf8 . Text.pack . show $ userId)
              ],
            requestBody = RequestBodyLBS body
          }

  result <- try $ httpLbs request manager
  case result of
    Left (e :: HttpException) ->
      pure $ Left $ ErrorDetail 0 (object ["error" .= show e])
    Right resp ->
      let respStatus = statusCode (responseStatus resp)
       in case respStatus of
            201 -> do
              case eitherDecode (responseBody resp) of
                Right (Object obj) ->
                  case parseMaybe (\o -> o .: "qualified_id" >>= (.: "id")) obj of
                    Just convId -> pure $ Right convId
                    Nothing ->
                      case parseMaybe (.: "id") obj of
                        Just convId -> pure $ Right convId
                        Nothing -> pure $ Left $ ErrorDetail respStatus (object ["error" .= ("Failed to extract conversation ID" :: Text)])
                Right _ -> pure $ Left $ ErrorDetail respStatus (object ["error" .= ("Invalid response format" :: Text)])
                Left err -> pure $ Left $ ErrorDetail respStatus (object ["error" .= Text.pack err])
            code ->
              pure $ Left $ ErrorDetail code (decodeResponse (responseBody resp))

associateChannelsToGroup ::
  Manager ->
  ApiUrl ->
  Token ->
  UserId ->
  UserGroupId ->
  [ConvId] ->
  IO (Either ErrorDetail ())
associateChannelsToGroup manager (ApiUrl apiUrl) (Token token) userId groupId convIds = do
  let url = apiUrl <> "/v12/user-groups/" <> show groupId <> "/channels?append_only=true"
      body = UpdateUserGroupChannels {channels = V.fromList convIds}

  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "PUT",
            requestHeaders =
              [ ("Authorization", "Bearer " <> Text.encodeUtf8 token),
                ("Content-Type", "application/json"),
                ("Z-User", Text.encodeUtf8 . Text.pack . show $ userId)
              ],
            requestBody = RequestBodyLBS (encode body)
          }

  result <- try $ httpLbs request manager
  case result of
    Left (e :: HttpException) ->
      pure $ Left $ ErrorDetail 0 (object ["error" .= show e])
    Right resp ->
      case statusCode (responseStatus resp) of
        200 -> pure $ Right ()
        code ->
          pure $ Left $ ErrorDetail code (decodeResponse (responseBody resp))

decodeResponse :: LBS.ByteString -> Value
decodeResponse body =
  case decode body of
    Just v -> v
    Nothing ->
      object
        [ "raw" .= Text.decodeUtf8 (LBS.toStrict body)
        ]
