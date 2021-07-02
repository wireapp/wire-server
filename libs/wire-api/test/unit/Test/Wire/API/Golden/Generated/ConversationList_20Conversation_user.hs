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
module Test.Wire.API.Golden.Generated.ConversationList_20Conversation_user where

import Data.Domain (Domain (..))
import Data.Id (Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import Data.Qualified (Qualified (..))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation
  ( Access (CodeAccess, LinkAccess, PrivateAccess),
    AccessRole
      ( ActivatedAccessRole,
        NonActivatedAccessRole,
        PrivateAccessRole,
        TeamAccessRole
      ),
    ConvMembers (ConvMembers, cmOthers, cmSelf),
    ConvType (ConnectConv, One2OneConv, RegularConv, SelfConv),
    Conversation (..),
    ConversationList (..),
    Member
      ( Member,
        memConvRoleName,
        memHidden,
        memHiddenRef,
        memId,
        memOtrArchived,
        memOtrArchivedRef,
        memOtrMuted,
        memOtrMutedRef,
        memOtrMutedStatus,
        memService
      ),
    MutedStatus (MutedStatus, fromMutedStatus),
    ReceiptMode (ReceiptMode, unReceiptMode),
  )
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

testObject_ConversationList_20Conversation_user_1 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_1 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            (fromJust (parseRoleName "71xuphsrwfoktrpiv4d08dxj6_1umizg67iisctw87gemvi114mtu"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvMessageTimer = Just (Ms {ms = 4760386328981119}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "gja5abzmdbher18aju2a5odl4"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 3500896164423997}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "8uujf_ntws83s6ndw2vtmfl_lvtjm8ryz8_s_vb"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "jyj4ssa0em95hapzqakg8dre3b2n3serk2rxycbu31h4qehsfwkcwsy4nd2cjazw3b2rlnr6xct7_j6h5tj7"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "ezb8hqfaiurv"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 4702135969825321}),
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "6ehv3qr8615hrwur4am"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Nothing,
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "cw7mbuwcp403kkn41os05211qyd4_kwo5yqog8vvqfph3efrsgqli_xzrk52_206s2tk9uyngsz4j03zqzmwihf83qgoxvt9g28kjq7u101r8l"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 7888194695541221}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            (fromJust (parseRoleName "rlvq6ir4ydawfd_xbw_g48u0z6nslo74nc05whuh8lirhoo7s6h"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 7607042969989865}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "u59pfu92v2v2bwy79i_t4vzc8n3vgkfu26j15annzdzip7rb0rxqmpbcp514oatnxxzb4neht40vx5fzxp453td4vmyvil_1ivyo1twrw1h34p48w5xtoih7hikle9w"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "k0eaeecf"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 7007725933930634}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "2zndnqqoy09lt9sv0yuiqp9seu0eyzm1rpd_"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 5891726360812026}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "ng9mkv_c44e_bf2sx1kc8z6nddk29xpccow72fmcmq4fw9qqkdy38m9bx7_n3qnwwisfjxvzqf56why"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 8461564964314645}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            }
        ],
      convHasMore = False
    }

testObject_ConversationList_20Conversation_user_2 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_2 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [CodeAccess, PrivateAccess],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "i6wt1ve04kf7cnif2ql0mx26pmv4x7i01nsd53lmxdhzdas11y6kzu3rc32tx2c4krryakjh79zdqg7nhdvpfisfvvip9a7oc8qjvmtqlm8y9t1stodu"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            }
        ],
      convHasMore = False
    }

testObject_ConversationList_20Conversation_user_3 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_3 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Nothing,
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "knvo5u3vhp8392gdab"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            }
        ],
      convHasMore = False
    }

testObject_ConversationList_20Conversation_user_4 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_4 = ConversationList {convList = [], convHasMore = True}

testObject_ConversationList_20Conversation_user_5 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_5 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) "",
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "2qzgr5wkn_ldz36qss3cwuwi7oqf_tqnoyvdpa3_g78ci97jnd6vdac5jh9i3narrk6xdxk9g1wntubp"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 1119369570957591}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "awjusc8ycco4z4tulpb22yvxi8bhj2v5r566om"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            (fromJust (parseRoleName "38lml9xkjpmsj719ju264ji_4zj8fn6jllcacwuvbc2jtazsm8"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 5569638481918792}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "14077ahakaln46bwhorrw07_o_y0ja14nz6ev2e7kja9y2q2p8bgj8pzep3ayn6c2o6ksxqoc4e45l4ilwof9hh_l5vzxr6"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 308227151936615}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "b6j6ik3lxqm6f7i3o5m4xaw78ca2q10jv0ein_ky4sbdl20gsdxq5c7c3d0wbarn8o3rmpjtu8qktjszo63jcdzq"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 7093551156016339}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "n6fl88imnu_da5t7w7s7"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 4373112121894721}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "xu0i3ibm5xk_4s3yo8j253zcx0r9fkzs4u4ft5m3idjbky2d_jk_orairyygciww9buthauyexxj7ii7p_a172r42n5wa3x7ji0"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "rwdp"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 8777284529043899}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "abxvawljqg8hsondqgphn8akwbj2rkzfrx0hsuvjjvfp8exd5w_g0aeuhats9633jc5h_byziq7ereq6wzzcvio3c6gd16fs4"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 4697061064867958}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "zvu0tvsjfpn3v9wzlxbjry6gebhfe826b4ywxdr6vxt4przgzxvbg7e803xjorr6hyk8pnq__gac8nwpme"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 4941125897110968}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "ll0_fnj8kds0ciu4gxcaaiwfwj26g896vhudwgdrj_v7pzs50_cs1nil5fbsa57qvf3qljeb3vldv10kdvx69wf"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 312342954800141}),
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "yzw8646ov72hhh_w2bhbz56wigwvli06mnli8dxegyl79zko8j0y0g2fvqtd_zq__zzzl8i6s3zy6lk80js_7z9k35u6xceo6_btyebbpgw4ivi301j9fsxq"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 8527199323798608}),
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                (parseRoleName "m8_lmrnkmpccq3xliv3mjvdbrqcaxjteav21_01txrkzue3dxkhsutmcjfd2mylai2t4")
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 829895852395824}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_6 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_6 = ConversationList {convList = [], convHasMore = True}

testObject_ConversationList_20Conversation_user_7 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_7 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                (parseRoleName "dv1d_nl54nkabi7vrfixhxo8fzyrt2ji0kp0668koza673_1__wmna4t2hl8twtatz9tjqnu")
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 1559334665101385}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            }
        ],
      convHasMore = False
    }

testObject_ConversationList_20Conversation_user_8 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_8 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvAccess = [PrivateAccess],
              cnvAccessRole = TeamAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "kklpzjg"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_9 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_9 = ConversationList {convList = [], convHasMore = True}

testObject_ConversationList_20Conversation_user_10 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_10 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "_9l9cqokichtgtjnvhtu32ss0ck6y48muyd69oomsb8713p_gost2_9jcg23j6lt5tbuu9qkpsapo2nm_5h4m4t1zenaoe349pzw"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 282891492942411}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "9r1618v8rx6b9f"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Nothing,
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "1ieq3sbt572tc4czhsdl1pf6vd3u4tf17z4p1hmu_ovnv8xjir7nczr6tz5u647z70wxwm41xex4it"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "71wzrfb9ty0rveunyh_moqu0g0e6ttkahj7mvl_pqwbfc9n8ohxto2wt6xxbfrcoi8kybqv0qgsgtgtimnbfj4r2fpms2eycvxmbt2yzgxa"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              cnvMessageTimer = Just (Ms {ms = 1406036180844894}),
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "un3yos7xctb371e8fku5qi9yfayz6keo8z38mpjcf96ttqoqrxk40456t1jlnt4iq6uaep5kq4kwn4bu4vg7zxgcbzouvpcipnj33kffl1e8itjt"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            }
        ],
      convHasMore = False
    }

testObject_ConversationList_20Conversation_user_11 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_11 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "crronx0noee9iatd0gh5r"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 6077261848303961}),
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "qa2h42sfabr2se34jysgbor858"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "o48adlocil8hrhc2yiydxkrhj9466c139d8jeau2zahe8bkfc2ao_hdowdnhwa82iz8cgdd1ilda7d9aoiuk5sevqk_upk2qq91331mh"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 4626765468655396}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            (fromJust (parseRoleName "5kh29zl85ii36dgzj60dmvswzi69ofij0o4d0mvioixrjs5nc5k395ix5gcr7"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 3172216229144519}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "q_co4y4_1hpcxpq50pcsrlxauuq25i0efge"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 3354972227724755}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "i4pr7249xf3ittu5s6r"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "m9qbh7lvffqb7k80cee"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "pi8v0uezeyjseqsjw6da9h74lbhgp1fc9d_rxk2n82pd5sqam7skankou8vi66kuqavz_11e4lqvw78qxfvfesvav7c"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 4699647814583777}),
              cnvReceiptMode = Nothing
            }
        ],
      convHasMore = False
    }

testObject_ConversationList_20Conversation_user_12 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_12 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "u14r7cx401i7qyhh9n868itmh316p67992oko18pfjacl8qe66ww9sl62qfohq4eapx7fg4fbonw7mm43naj21csjzos5rvgppli8927a8m38_yh0a5"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 8314115697628639}),
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "u78l36rd2zqd4"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "famszv2ugu59f0d53o8i7hjc092bwqe8tccwhvl0aappyemhfs3sbc94wo3_oentu_wqzmn8hmrb"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 4496481998290983}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "0juii56qpd0864vcj"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 6904576004663823}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "n5q4dhulov8_ms5zpw5d6fl_x4jilyq1rxawgpomq78zrwslui8s88d50w3eblxom0qb2gujjb8u970o7ah1wc0k300"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 1823461342077434}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "gxhbc2t7q6rrcftfwmg3jse0gy8fyw0iw84bv58kr76d49o17h8mnggopexntbij12qafzyz1o2ep52cf"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              cnvMessageTimer = Just (Ms {ms = 2685267653752513}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_13 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_13 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "_l7oykl7ycvdlba5pe73y9epds"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "_hy7i5xm_11rafcrfebghyi906l330ufunaiyo6k0alofoory6hl7h4xjt75o1_a7pt2xnwcbudqbka4csvzbpbnlllngukfah8bqx7zcoq3"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "utby2ypvyrwlys82_edw23ytrpj"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "w4vn9eu815hobipm_n3da9jhd7yb9ny_f47_0zlk8jrluhdvk0y39fvczidx3q848tc3s_o8cfiuon"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 1294477963626888}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_14 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_14 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "lgd71k4qvxtih"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Nothing,
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "r_gc0fohzp_yawd_k1c4tofh_m0ngos87kgxcacjyde1n1js9knbrq5ytdzns3rtyq9k0yciwn7xagtefk7b"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 2256129941904898}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "ohfwggqn9gstvw220280kws34kjr3fa51mg8babmrist1_tgins9q4iwjz4jwg8hzfa4glbqe9c5df1"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 7574050251117485}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "n743vdsooo4ajvp9t_sok1n9i"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvMessageTimer = Just (Ms {ms = 671261490527917}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "m27atvp24yytpx19vp2_2e83pk27eievke6k7d2v1qgs2rncfdz5spzaq6ngt2hg4kobual7bhb29q1npixng71v05o"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "b3e571bd4l2sxhdnm_4r0hpznzish0m0zudnoh"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 1925949395505672}),
              cnvReceiptMode = Nothing
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "bzkw84_boyuhiqe51_ancwl1bx6c_sl5usf_6xf39dmgvui2uvz87_9xr42_sg8wqokxge_hkmyjo6dhu3des"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 4416586316876087}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            (fromJust (parseRoleName "6l9clln0mj4nu7emkz3ds07iiyduug29o33ecg66g02of2t8xd3llb09vz3d95"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 5544703373031406}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            (fromJust (parseRoleName "tak8j21oh9iuwj1ivx7717hs5iokypsmm_zgfln47wvc4v2pshgl5k3cmsv3_vj8"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 7382109943964396}),
              cnvReceiptMode = Nothing
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_15 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_15 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvAccess = [CodeAccess, LinkAccess, PrivateAccess, PrivateAccess, CodeAccess],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "k1lnvywuwtpvcblff04ediu27__le7is9hj1fsp7sx9ba8tjwa0zllzrr21g_9ek9joqzjwyiit7drvnylthvmbyepvs_cua7"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 6813643981796860}),
              cnvReceiptMode = Nothing
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_16 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_16 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "5xme3oh72w6fjk_hz6ktni1qol3jdamb704o24oi4z4l09u"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 5060646367946340}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "a7gz0gjpe"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "7sbkqx73wmlxh7jzclg7ugfne6t0oiw"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "7_j2lhoa8blqpxe6ieoxgj13g44fc3os5c0hzdw0vkicytmm38c1ti44jtwk0awi1p8h3j21ayaoxhgikdf8aqk92sxf"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvMessageTimer = Just (Ms {ms = 4425540032419297}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_17 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_17 = ConversationList {convList = [], convHasMore = False}

testObject_ConversationList_20Conversation_user_18 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_18 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "o8_"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 8298377768402548}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "u_xhbk"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "cl5sp8vi0rzm5nzkg3v3518zc5kixfqvc35qi"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 4445775551954449}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                (parseRoleName "_moxeapv0m19z7oqm15_r00uzfaelz3ql_k6j3ceccc0mk8h8icj2m6dkvx0v_d0vltz318")
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Just (Ms {ms = 7034560312247301}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_19 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_19 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = True,
                          memOtrMutedStatus = Nothing,
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "39cavf2gxmdivwom4sbqacf5qid3jeh2_yon0puszvkl4e9rof60k4f9t48xqme95zv0zhn4z5m98_x0g9eig6wlj04is6zvwg6byi7z07c6gt9090ny2u3lt"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = RegularConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              cnvAccess = [],
              cnvAccessRole = NonActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Nothing,
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "5h13g6f13e8vecpsadfwntn970iek5v4zl_421qw1p6_v0dudbh1ac_h70og_d7ed4m_q1trwmk9evrzqeq2s1llbwh7mn6ar_g1mikt6s"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Nothing,
              cnvMessageTimer = Nothing,
              cnvReceiptMode = Nothing
            }
        ],
      convHasMore = True
    }

testObject_ConversationList_20Conversation_user_20 :: ConversationList Conversation
testObject_ConversationList_20Conversation_user_20 =
  ConversationList
    { convList =
        [ Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = PrivateAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Nothing,
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "smh85kmu7ryn_0gf0oo5y5xlwpl714k2anilkpdl3lpn_gm3bh4oe4uk_y2sfizmh6odeuyw4odrvlwq0rc8lvsrtv31uqtvdpuadokpzfz8ho53c4kb1ify606d6ou"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 7973507983724794}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "g286jwpeoptn5z1yaxa_zij_buu4fch4plgoz6v59ai1zu_rxc__0le2zxamri3jsgq0tiiwxcb29gkmds2q2zl_pc37w9ikexf2s44ynyzs"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 713209987069426}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                          memService =
                            Just
                              ( ServiceRef
                                  { _serviceRefId =
                                      (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                                    _serviceRefProvider =
                                      (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                                  }
                              ),
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName = (fromJust (parseRoleName "lvva20nz949u9uoh6d653ukgy_qtpugz40j6122"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              cnvMessageTimer = Just (Ms {ms = 5611100743084448}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) (Domain ""),
              cnvType = ConnectConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = True,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Nothing,
                          memHidden = True,
                          memHiddenRef = Nothing,
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "nm0c0tgty7ycwx2sazcmpcubcf_oh6iqufse0ml5u7cl3xxbjzyir8li6iew8t3agg5isko50ha58m9lv5vj933vwrxvpsv"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 3460496531316381}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) (Domain ""),
              cnvType = One2OneConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              cnvAccess = [],
              cnvAccessRole = TeamAccessRole,
              cnvName = Just "",
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = False,
                          memOtrArchivedRef = Just "",
                          memHidden = False,
                          memHiddenRef = Just "",
                          memConvRoleName =
                            ( fromJust
                                ( parseRoleName
                                    "nk8qm1m6euslkb4j1x0cs18mfffhmtv9sg7eyudgbd3b2ysqnlr1jkted9s7fhd8v93irig2pk_v4p12_lwu52ml3r2a1eb1gxqrakupmrdp_wm"
                                )
                            )
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              cnvMessageTimer = Just (Ms {ms = 2052301235302529}),
              cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
            },
          Conversation
            { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) (Domain ""),
              cnvType = SelfConv,
              cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              cnvAccess = [],
              cnvAccessRole = ActivatedAccessRole,
              cnvName = Nothing,
              cnvMembers =
                ConvMembers
                  { cmSelf =
                      Member
                        { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                          memService = Nothing,
                          memOtrMuted = False,
                          memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                          memOtrMutedRef = Just "",
                          memOtrArchived = True,
                          memOtrArchivedRef = Just "",
                          memHidden = True,
                          memHiddenRef = Just "",
                          memConvRoleName = (fromJust (parseRoleName "ahcbvp03bcrsmxc1rmnripjsaxw99dpw"))
                        },
                    cmOthers = []
                  },
              cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              cnvMessageTimer = Just (Ms {ms = 7068596443593491}),
              cnvReceiptMode = Nothing
            }
        ],
      convHasMore = True
    }
