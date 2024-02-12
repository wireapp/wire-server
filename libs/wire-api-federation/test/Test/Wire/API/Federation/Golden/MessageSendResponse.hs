-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Federation.Golden.MessageSendResponse where

import Data.Domain (Domain (Domain))
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.UUID as UUID
import GHC.Exts (IsList (fromList))
import Imports
import Wire.API.Federation.API.Galley (MessageSendResponse (..))
import Wire.API.Message
import Wire.API.Routes.Public.Galley.Messaging
import Wire.API.User.Client (QualifiedUserClients (..))

missing :: QualifiedUserClients
missing =
  QualifiedUserClients
    { qualifiedUserClients =
        fromList
          [ ( Domain "golden.example.com",
              fromList
                [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002")),
                    fromList [ClientId 0, ClientId 1]
                  ),
                  ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000")),
                    fromList [ClientId 0]
                  )
                ]
            )
          ]
    }

redundant :: QualifiedUserClients
redundant =
  QualifiedUserClients
    { qualifiedUserClients =
        fromList
          [ ( Domain "golden.example.com",
              fromList
                [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000003")),
                    fromList [ClientId 0, ClientId 1]
                  ),
                  ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000004")),
                    fromList [ClientId 0]
                  )
                ]
            )
          ]
    }

deleted :: QualifiedUserClients
deleted =
  QualifiedUserClients
    { qualifiedUserClients =
        fromList
          [ ( Domain "golden.example.com",
              fromList
                [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000005")),
                    fromList [ClientId 0, ClientId 1]
                  ),
                  ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000006")),
                    fromList [ClientId 0]
                  )
                ]
            )
          ]
    }

failed :: QualifiedUserClients
failed =
  QualifiedUserClients
    { qualifiedUserClients =
        fromList
          [ ( Domain "golden.example.com",
              fromList
                [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000007")),
                    fromList [ClientId 0, ClientId 1]
                  ),
                  ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000008")),
                    fromList [ClientId 0]
                  )
                ]
            )
          ]
    }

failedToConfirm :: QualifiedUserClients
failedToConfirm =
  QualifiedUserClients
    { qualifiedUserClients =
        fromList
          [ ( Domain "golden.example.com",
              fromList
                [ ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000009")),
                    fromList [ClientId 0, ClientId 1]
                  ),
                  ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000010")),
                    fromList [ClientId 0]
                  )
                ]
            )
          ]
    }

testObject_MessageSendResponse1 :: MessageSendResponse
testObject_MessageSendResponse1 =
  MessageSendResponse $
    Right
      MessageSendingStatus
        { mssTime = toUTCTimeMillis (read "1864-04-12 12:22:43.673 UTC"),
          mssMissingClients = missing,
          mssRedundantClients = redundant,
          mssDeletedClients = deleted,
          mssFailedToSend = failed,
          mssFailedToConfirmClients = failedToConfirm
        }

testObject_MessageSendResponse2 :: MessageSendResponse
testObject_MessageSendResponse2 = MessageSendResponse . Left $ MessageNotSentLegalhold

testObject_MessageSendResponse3 :: MessageSendResponse
testObject_MessageSendResponse3 =
  MessageSendResponse . Left . MessageNotSentClientMissing $
    MessageSendingStatus
      { mssTime = toUTCTimeMillis (read "1864-04-12 12:22:43.673 UTC"),
        mssMissingClients = missing,
        mssRedundantClients = redundant,
        mssDeletedClients = deleted,
        mssFailedToSend = failed,
        mssFailedToConfirmClients = failedToConfirm
      }

testObject_MessageSendResponse4 :: MessageSendResponse
testObject_MessageSendResponse4 = MessageSendResponse . Left $ MessageNotSentConversationNotFound

testObject_MessageSendResponse5 :: MessageSendResponse
testObject_MessageSendResponse5 = MessageSendResponse . Left $ MessageNotSentUnknownClient

testObject_MessageSendResponse6 :: MessageSendResponse
testObject_MessageSendResponse6 = MessageSendResponse . Left $ MessageNotSentLegalholdOldClients
