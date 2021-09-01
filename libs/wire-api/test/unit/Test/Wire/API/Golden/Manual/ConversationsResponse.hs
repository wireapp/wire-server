module Test.Wire.API.Golden.Manual.ConversationsResponse
  ( testObject_ConversationsResponse_1,
  )
where

import Data.Domain
import Data.Id (Id (Id))
import Data.Misc
import Data.Qualified
import qualified Data.UUID as UUID
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Role

testObject_ConversationsResponse_1 :: ConversationsResponse
testObject_ConversationsResponse_1 =
  ConversationsResponse
    { crFound = [conv1, conv2],
      crNotFound =
        [ Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))) (Domain "golden.example.com"),
          Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-111111111112"))) (Domain "golden2.example.com")
        ],
      crFailed =
        [ Qualified (Id (fromJust (UUID.fromString "00000018-4444-0020-0000-000e00000002"))) (Domain "golden.example.com"),
          Qualified (Id (fromJust (UUID.fromString "99999999-0000-0020-0000-111111111112"))) (Domain "golden3.example.com")
        ]
    }

conv1 :: Conversation
conv1 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) (Domain "golden.example.com"),
      cnvType = One2OneConv,
      cnvCreator = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001")),
      cnvAccess = [],
      cnvAccessRole = PrivateAccessRole,
      cnvName = Just " 0",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
                  memService = Nothing,
                  memOtrMuted = True,
                  memOtrMutedStatus = Nothing,
                  memOtrMutedRef = Nothing,
                  memOtrArchived = False,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName = fromJust (parseRoleName "rhhdzf0j0njilixx0g0vzrp06b_5us")
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
      cnvMessageTimer = Nothing,
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})
    }

conv2 :: Conversation
conv2 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))) (Domain "golden.example.com"),
      cnvType = SelfConv,
      cnvCreator = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")),
      cnvAccess =
        [ InviteAccess,
          InviteAccess,
          CodeAccess,
          LinkAccess,
          InviteAccess,
          PrivateAccess,
          LinkAccess,
          CodeAccess,
          CodeAccess,
          LinkAccess,
          PrivateAccess,
          InviteAccess
        ],
      cnvAccessRole = NonActivatedAccessRole,
      cnvName = Just "",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
                  memService = Nothing,
                  memOtrMuted = True,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                  memOtrMutedRef = Nothing,
                  memOtrArchived = False,
                  memOtrArchivedRef = Nothing,
                  memHidden = True,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    fromJust (parseRoleName "9b2d3thyqh4ptkwtq2n2v9qsni_ln1ca66et_z8dlhfs9oamp328knl3rj9kcj")
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),
      cnvMessageTimer = Just (Ms {ms = 1319272593797015}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})
    }
