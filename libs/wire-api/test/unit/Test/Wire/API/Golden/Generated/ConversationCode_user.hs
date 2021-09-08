{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.ConversationCode_user where

import Data.Code (Key (Key, asciiKey), Value (Value, asciiValue))
import Data.Coerce (coerce)
import Data.Misc (HttpsUrl (HttpsUrl))
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import Imports (Maybe (Just, Nothing), fromRight, undefined)
import URI.ByteString
  ( Authority
      ( Authority,
        authorityHost,
        authorityPort,
        authorityUserInfo
      ),
    Host (Host, hostBS),
    Query (Query, queryPairs),
    Scheme (Scheme, schemeBS),
    URIRef
      ( URI,
        uriAuthority,
        uriFragment,
        uriPath,
        uriQuery,
        uriScheme
      ),
  )
import Wire.API.Conversation.Code (ConversationCode (..))

testObject_ConversationCode_user_1 :: ConversationCode
testObject_ConversationCode_user_1 =
  ConversationCode
    { conversationKey = Key {asciiKey = unsafeRange (fromRight undefined (validate "M0vnbETaqAgL8tv5Z1_x"))},
      conversationCode = Value {asciiValue = unsafeRange (fromRight undefined (validate "sEG3Y60tIsd9P3"))},
      conversationUri =
        Just
          ( coerce
              URI
                { uriScheme = Scheme {schemeBS = "https"},
                  uriAuthority =
                    Just
                      ( Authority
                          { authorityUserInfo = Nothing,
                            authorityHost = Host {hostBS = "example.com"},
                            authorityPort = Nothing
                          }
                      ),
                  uriPath = "",
                  uriQuery = Query {queryPairs = []},
                  uriFragment = Nothing
                }
          )
    }

testObject_ConversationCode_user_2 :: ConversationCode
testObject_ConversationCode_user_2 =
  ConversationCode
    { conversationKey = Key {asciiKey = unsafeRange (fromRight undefined (validate "NEN=eLUWHXclTp=_2Nap"))},
      conversationCode = Value {asciiValue = unsafeRange (fromRight undefined (validate "lLz-9vR8ENum0kI-xWJs"))},
      conversationUri = Nothing
    }
