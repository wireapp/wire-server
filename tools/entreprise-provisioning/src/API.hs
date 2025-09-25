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
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Imports
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Types

createChannel ::
  Manager ->
  Services ->
  Token ->
  UserId ->
  TeamId ->
  ChannelName ->
  IO (Either ErrorDetail ConvId)
createChannel manager services (Token token) userId teamId channelName = do
  let url = services.galleyUrl.fromApiUrl <> "/v12/conversations"
      body =
        object
          [ "qualified_users" .= ([] :: [Value]),
            "name" .= channelName,
            "access" .= ([] :: [Text]),
            "access_role" .= ["team_member" :: Text],
            "team" .= object ["teamid" .= teamId, "managed" .= False],
            "message_timer" .= Null,
            "receipt_mode" .= Null,
            "protocol" .= ("mls" :: Text),
            "group_conv_type" .= ("channel" :: Text)
          ]

  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = "POST",
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
        201 -> do
          case eitherDecode (responseBody resp) of
            Right (Object obj) ->
              case parseMaybe (\o -> o .: "qualified_id" >>= (.: "id")) obj of
                Just convId -> pure $ Right convId
                Nothing ->
                  case parseMaybe (.: "id") obj of
                    Just convId -> pure $ Right convId
                    Nothing -> pure $ Left $ ErrorDetail 201 (object ["error" .= ("Failed to extract conversation ID" :: Text)])
            Right _ -> pure $ Left $ ErrorDetail 201 (object ["error" .= ("Invalid response format" :: Text)])
            Left err -> pure $ Left $ ErrorDetail 201 (object ["error" .= Text.pack err])
        code ->
          pure $ Left $ ErrorDetail code (decodeResponse (responseBody resp))

associateChannelsToGroup ::
  Manager ->
  Services ->
  Token ->
  UserId ->
  UserGroupId ->
  [ConvId] ->
  IO (Either ErrorDetail ())
associateChannelsToGroup manager services (Token token) userId groupId convIds = do
  let url = services.brigUrl.fromApiUrl <> "/v12/user-groups/" <> show groupId <> "/channels?append_only=true"
      body = object ["channels" .= convIds]

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
