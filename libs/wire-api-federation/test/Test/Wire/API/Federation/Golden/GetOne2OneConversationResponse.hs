module Test.Wire.API.Federation.Golden.GetOne2OneConversationResponse where

import Data.Domain
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley
import Wire.API.MLS.Keys
import Wire.API.Routes.Versioned qualified as ClientAPI

testObject_GetOne2OneConversationResponseOk :: GetOne2OneConversationResponse
testObject_GetOne2OneConversationResponseOk =
  GetOne2OneConversationOk remoteConversation

testObject_GetOne2OneConversationResponseBackendMismatch :: GetOne2OneConversationResponse
testObject_GetOne2OneConversationResponseBackendMismatch = GetOne2OneConversationBackendMismatch

testObject_GetOne2OneConversationResponseNotConnected :: GetOne2OneConversationResponse
testObject_GetOne2OneConversationResponseNotConnected = GetOne2OneConversationNotConnected

testObject_GetOne2OneConversationResponseV2Ok :: GetOne2OneConversationResponseV2
testObject_GetOne2OneConversationResponseV2Ok =
  GetOne2OneConversationV2Ok $
    RemoteMLSOne2OneConversation
      { conversation = remoteConversationV2,
        publicKeys =
          MLSKeysByPurpose
            { removal =
                MLSKeys
                  { ed25519 =
                      MLSPublicKey
                        (fromBase64TextLenient "7C8PpP91rzMnD4VHuWTI3yNuInfbzIk937uF0Cg/Piw="),
                    ecdsa_secp256r1_sha256 =
                      MLSPublicKey
                        (fromBase64TextLenient "ArUTSywmqya1wAGwrK+pJuA7KSpKm06y3eZq8Py2NMM="),
                    ecdsa_secp384r1_sha384 =
                      MLSPublicKey
                        (fromBase64TextLenient "7pKiTLf72OfpQIeVeXF0mJKfWsBnhTtMUy0zuKasYjlTQUW5fGtcyAFXinM3FahV"),
                    ecdsa_secp521r1_sha512 =
                      MLSPublicKey
                        (fromBase64TextLenient "9twvhZ57ytiujWXFtSmxd8I5r9iZjgdCtGtReJT3yQL2BCGZ80Vzq/MrmV+O0i7lZEI1gqbr8vL1xKk+2h2LyQ==")
                  }
            }
      }

testObject_GetOne2OneConversationResponseV2BackendMismatch :: GetOne2OneConversationResponseV2
testObject_GetOne2OneConversationResponseV2BackendMismatch = GetOne2OneConversationV2BackendMismatch

testObject_GetOne2OneConversationResponseV2NotConnected :: GetOne2OneConversationResponseV2
testObject_GetOne2OneConversationResponseV2NotConnected = GetOne2OneConversationV2NotConnected

remoteConversation :: RemoteConversation
remoteConversation =
  RemoteConversation
    { id = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200040001"))),
      metadata =
        ConversationMetadata
          { cnvmType = One2OneConv,
            cnvmCreator = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))),
            cnvmAccess = [],
            cnvmAccessRoles = Set.empty,
            cnvmName = Just " 0",
            cnvmTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
            cnvmMessageTimer = Nothing,
            cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = -2})
          },
      members =
        RemoteConvMembers
          { selfRole = roleNameWireAdmin,
            others =
              [ OtherMember
                  { omQualifiedId =
                      Qualified
                        (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001")))
                        (Domain "example.com"),
                    omService = Nothing,
                    omConvRoleName = roleNameWireMember
                  }
              ]
          },
      protocol =
        ClientAPI.Versioned . ProtocolMLS $
          ConversationMLSData
            { cnvmlsGroupId = GroupId "group",
              cnvmlsActiveData = Nothing
            }
    }

remoteConversationV2 :: RemoteConversationV2
remoteConversationV2 =
  RemoteConversationV2
    { id = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200040001"))),
      metadata =
        ConversationMetadata
          { cnvmType = One2OneConv,
            cnvmCreator = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))),
            cnvmAccess = [],
            cnvmAccessRoles = Set.empty,
            cnvmName = Just " 0",
            cnvmTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
            cnvmMessageTimer = Nothing,
            cnvmReceiptMode = Just (ReceiptMode {unReceiptMode = -2})
          },
      members =
        RemoteConvMembers
          { selfRole = roleNameWireAdmin,
            others =
              [ OtherMember
                  { omQualifiedId =
                      Qualified
                        (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001")))
                        (Domain "example.com"),
                    omService = Nothing,
                    omConvRoleName = roleNameWireMember
                  }
              ]
          },
      protocol =
        ProtocolMLS $
          ConversationMLSData
            { cnvmlsGroupId = GroupId "group",
              cnvmlsActiveData = Nothing
            }
    }
