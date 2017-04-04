{-# LANGUAGE OverloadedStrings #-}

module Galley.API.Error where

import Data.Text.Lazy
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error

convNotFound :: Error
convNotFound = Error status404 "no-conversation" "conversation not found"

invalidSelfOp :: Error
invalidSelfOp = invalidOp "invalid operation for self conversation"

invalidOne2OneOp :: Error
invalidOne2OneOp = invalidOp "invalid operation for 1:1 conversations"

invalidConnectOp :: Error
invalidConnectOp = invalidOp "invalid operation for connect conversation"

invalidOp :: Text -> Error
invalidOp = Error status403 "invalid-op"

invalidPayload :: Text -> Error
invalidPayload = Error status400 "invalid-payload"

notConnected :: Error
notConnected = Error status403 "not-connected" "Users are not connected"

tooManyMembers :: Error
tooManyMembers = Error status403 "too-many-members" "Maximum number of members per conversation reached"

accessDenied :: Error
accessDenied = Error status403 "access-denied" "Conversation access denied"

invalidUUID4 :: Error
invalidUUID4 = Error status400 "client-error" "Invalid UUID v4 format"

unknownClient :: Error
unknownClient = Error status403 "unknown-client" "Sending client not known"
