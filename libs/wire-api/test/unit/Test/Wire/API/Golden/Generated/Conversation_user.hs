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
module Test.Wire.API.Golden.Generated.Conversation_user where

import Data.Domain
import Data.Id (Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import Data.Qualified
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_Conversation_user_1 :: Conversation
testObject_Conversation_user_1 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) (Domain "golden.example.com"),
      cnvType = One2OneConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))),
      cnvAccess = [],
      cnvAccessRole = PrivateAccessRole,
      cnvName = Just " 0",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                  memService = Nothing,
                  memOtrMuted = True,
                  memOtrMutedStatus = Nothing,
                  memOtrMutedRef = Nothing,
                  memOtrArchived = False,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName = (fromJust (parseRoleName "rhhdzf0j0njilixx0g0vzrp06b_5us"))
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
      cnvMessageTimer = Nothing,
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})
    }

testObject_Conversation_user_2 :: Conversation
testObject_Conversation_user_2 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))) (Domain "golden.example.com"),
      cnvType = SelfConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
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
                { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                  memService = Nothing,
                  memOtrMuted = True,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                  memOtrMutedRef = Nothing,
                  memOtrArchived = False,
                  memOtrArchivedRef = Nothing,
                  memHidden = True,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    (fromJust (parseRoleName "9b2d3thyqh4ptkwtq2n2v9qsni_ln1ca66et_z8dlhfs9oamp328knl3rj9kcj"))
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),
      cnvMessageTimer = Just (Ms {ms = 1319272593797015}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})
    }

testObject_Conversation_user_3 :: Conversation
testObject_Conversation_user_3 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))) (Domain "golden.example.com"),
      cnvType = One2OneConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),
      cnvAccess = [],
      cnvAccessRole = ActivatedAccessRole,
      cnvName = Just "\994543",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                          }
                      ),
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
                            "c6vuntjr9gwiigc0_cjtg2eysqquizmi27hr5rcpb273ox_f3r68r51jxqqctu08xd0gbvwwmekpo7yo_duaqh8pcrdh3uk_oogx6ol6kkq9wg3252kx5w9_r"
                        )
                    )
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))),
      cnvMessageTimer = Just (Ms {ms = 158183656363340}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 0})
    }

testObject_Conversation_user_4 :: Conversation
testObject_Conversation_user_4 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))) (Domain "golden.example.com"),
      cnvType = ConnectConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
      cnvAccess = [],
      cnvAccessRole = PrivateAccessRole,
      cnvName = Just "\NAK-J",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
                          }
                      ),
                  memOtrMuted = True,
                  memOtrMutedStatus = Nothing,
                  memOtrMutedRef = Nothing,
                  memOtrArchived = True,
                  memOtrArchivedRef = Just "",
                  memHidden = True,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    (fromJust (parseRoleName "m123vuiwu65elqzh2xslj7koh_hoaozqcokprzujft6k_g_uv1hwo7xts"))
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "r1rg526serx51g15n99y1bw_9q0qrcwck3jxl7ocjsjqcoux7d1zbkz9nnczy92t2oyogxrx3cyh_b8yv44l61mx9uzdnv6"
                          )
                      )
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))),
      cnvMessageTimer = Nothing,
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
    }

testObject_Conversation_user_5 :: Conversation
testObject_Conversation_user_5 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))) (Domain "golden.example.com"),
      cnvType = ConnectConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))),
      cnvAccess = [],
      cnvAccessRole = PrivateAccessRole,
      cnvName = Just "'",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
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
                            "xvfvxp6h5e0sngt_bnwfa4tyn1lw028rzrxhnuz1mxgyi1ftcj7o9hilr4qo_ir59q9gktkdb6qmmyvju1n9l6ev4vh2clfi7whq4uxtq"
                        )
                    )
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
                    omService = Nothing,
                    omConvRoleName = (fromJust (parseRoleName "94maa8a519kifbmlwehm5sxmkuokr6"))
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "lmzpvgv4f8kt1wzdmecu8aqvnfv5l0cs0x1odmpdvaz25u2kofhywz92kx7mfxvld99im98_ksi0feski60eq63nlwtst2_ud5r2bi3k"
                          )
                      )
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
                    omService = Nothing,
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "8euvagsxew2ds5r8yiy_soqa2yhy12oi9ljyxmcm40j_oxt4i0q1rsd3twu43af9q6fotbrzeyjktmewqehafl6ax9372wxcg4r5"
                          )
                      )
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                            }
                        ),
                    omConvRoleName =
                      (fromJust (parseRoleName "leerha8jseiakvd1pdzoq0sjf6bq1_yxepvf62d_jurktowqwiyswks3fhgm"))
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "n0txwqcuxeq_76cffv3mc4lbddiqtyjzrklf93yfcrw6mmhqoa3na5dm_egdgiflqt29v6t61n32qvvujtk_gs1iue0dbsldj0"
                          )
                      )
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "ughz4ajb27yd55w_i9idbgelgut_ksa4pj0k1iwuwgstmwc0ly9_pt1zr3fs1vqph1fzobfccklzmdam_6dbiktrpriqpad8itw4ezzah6d8e27w8xe7751xztz_b"
                          )
                      )
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))),
      cnvMessageTimer = Just (Ms {ms = 3545846644696388}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
    }

testObject_Conversation_user_6 :: Conversation
testObject_Conversation_user_6 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) (Domain "golden.example.com"),
      cnvType = RegularConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
      cnvAccess = [PrivateAccess],
      cnvAccessRole = ActivatedAccessRole,
      cnvName = Just "",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                  memService = Nothing,
                  memOtrMuted = False,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                  memOtrMutedRef = Just "",
                  memOtrArchived = True,
                  memOtrArchivedRef = Just "",
                  memHidden = True,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    (fromJust (parseRoleName "7yrq155bja2p68pkx0ze6lu_i9paws_55wd89qsdghna3muu9eryz4wfu8"))
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                            }
                        ),
                    omConvRoleName = (fromJust (parseRoleName "iw0eer5er3zfvoqdlo"))
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),
      cnvMessageTimer = Just (Ms {ms = 8467521308908805}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})
    }

testObject_Conversation_user_7 :: Conversation
testObject_Conversation_user_7 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))) (Domain "golden.example.com"),
      cnvType = SelfConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      cnvAccess = [PrivateAccess, CodeAccess],
      cnvAccessRole = ActivatedAccessRole,
      cnvName = Nothing,
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                  memService = Nothing,
                  memOtrMuted = True,
                  memOtrMutedStatus = Nothing,
                  memOtrMutedRef = Just "",
                  memOtrArchived = True,
                  memOtrArchivedRef = Nothing,
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName = (fromJust (parseRoleName "jfrshedq51a"))
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
                    omService = Nothing,
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "7_boa_ycz2wuxjejoukgch7q0ity8k2k7sd6gn_rkk5l6_m4dpmlx0k4klo6mdvc11noo78qeo7d_n05lojjs9"
                          )
                      )
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))),
      cnvMessageTimer = Just (Ms {ms = 118554855340166}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
    }

testObject_Conversation_user_8 :: Conversation
testObject_Conversation_user_8 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))) (Domain "golden.example.com"),
      cnvType = SelfConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))),
      cnvAccess = [InviteAccess, PrivateAccess, PrivateAccess, InviteAccess, InviteAccess, PrivateAccess, LinkAccess],
      cnvAccessRole = TeamAccessRole,
      cnvName = Nothing,
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                          }
                      ),
                  memOtrMuted = False,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                  memOtrMutedRef = Just "",
                  memOtrArchived = True,
                  memOtrArchivedRef = Just "",
                  memHidden = True,
                  memHiddenRef = Just "",
                  memConvRoleName = (fromJust (parseRoleName "othyp2hs"))
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "fl2vgxnc40qnxz7eivgmb9uer3y_mtfk0whgu5tv4m108ftmryr4ji5duw2srp_7gh73y46f6krak3ef0by6fnko4rnxodby2voxfgb6u05k6z1hwgh4j8ce_as"
                          )
                      )
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
      cnvMessageTimer = Just (Ms {ms = 5923643994342681}),
      cnvReceiptMode = Nothing
    }

testObject_Conversation_user_9 :: Conversation
testObject_Conversation_user_9 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))) (Domain "golden.example.com"),
      cnvType = RegularConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),
      cnvAccess = [PrivateAccess, InviteAccess, LinkAccess, LinkAccess, InviteAccess, LinkAccess, CodeAccess],
      cnvAccessRole = TeamAccessRole,
      cnvName = Nothing,
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                          }
                      ),
                  memOtrMuted = True,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                  memOtrMutedRef = Just "",
                  memOtrArchived = True,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    ( fromJust
                        (parseRoleName "xpb7s51x2p54ban1kuq9d5bkwfnmep835yf1r1azgbrusdn12xpi13estxii5t4cval1qkwuskt9yc")
                    )
                },
            cmOthers = []
          },
      cnvTeam = Nothing,
      cnvMessageTimer = Just (Ms {ms = 3783180688855389}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
    }

testObject_Conversation_user_10 :: Conversation
testObject_Conversation_user_10 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))) (Domain "golden.example.com"),
      cnvType = ConnectConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))),
      cnvAccess = [CodeAccess, PrivateAccess, InviteAccess],
      cnvAccessRole = NonActivatedAccessRole,
      cnvName = Just "",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                          }
                      ),
                  memOtrMuted = True,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                  memOtrMutedRef = Nothing,
                  memOtrArchived = False,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    ( fromJust
                        ( parseRoleName
                            "f6i7jiywt5z70w0_2uo5nzl6a7jeb8uij_mlvzkutbuzmuv_kfgl4myu_wh5bjkhbm0qdzacid1zytxvl8jjzyxn3u29enr20563j1mx0cm6vayj"
                        )
                    )
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "8jz06l_sqgy5jsy2tkj36fx2xtkdhuwhuiktpq2trp9i438bk4xw1lzoi3aysancdc15ihj6r5kr67tkw9hsbhaybyv1356wnfkqsdkzo4kgc"
                          )
                      )
                  }
              ]
          },
      cnvTeam = Nothing,
      cnvMessageTimer = Just (Ms {ms = 1131988659409974}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})
    }

testObject_Conversation_user_11 :: Conversation
testObject_Conversation_user_11 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))) (Domain "golden.example.com"),
      cnvType = SelfConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))),
      cnvAccess = [LinkAccess],
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
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                          }
                      ),
                  memOtrMuted = True,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                  memOtrMutedRef = Just "",
                  memOtrArchived = True,
                  memOtrArchivedRef = Nothing,
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    ( fromJust
                        ( parseRoleName
                            "eq24_qppnbjp8nxeda3bgg62wy7uviku7tugpzds1sh_rhois7of0ht1yr37ytdgntv9iz_mmvpxd1sl6uwjj75yehuskmxdnsow6wxi08mykn0lgcal5fix28dd0"
                        )
                    )
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) domain,
                    omService = Nothing,
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "7yp6ch28sx90qjew7i2oa6f3a0a67xtkmef1ronl_lmf7u0lve4z6468jswcqkq7ovr48idryq7dqurpehzzl262oqnoi3bj2_"
                          )
                      )
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "ogmflb36qx1edb7q8wxacedus_2ppb6pu2vzph4fnhlc_x3kf271v1x127vin878egys54n3hkgs315xo3ufylmom8v25g3snrauoyxmta_iz"
                          )
                      )
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "if7jp_ofhfnlx9amsxhapq21fipzxyg0n1fvawnot0z67qbx_2rgu768eq13gyrtv_35y7kx7nizjbmea6mxg8bf9vl_k9fq7bwlzxty"
                          )
                      )
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                            }
                        ),
                    omConvRoleName = (fromJust (parseRoleName "1p2vr4mofv6q14vovgx5fnqh_ux"))
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) domain,
                    omService = Nothing,
                    omConvRoleName = (fromJust (parseRoleName "b61vxuzhoqvvdl6"))
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))),
      cnvMessageTimer = Just (Ms {ms = 2882038444751786}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})
    }

testObject_Conversation_user_12 :: Conversation
testObject_Conversation_user_12 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))) (Domain "golden.example.com"),
      cnvType = ConnectConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
      cnvAccess = [],
      cnvAccessRole = TeamAccessRole,
      cnvName = Just "",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
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
                            "svdfjv4ckfm7gre62yh0l1i7x5k49wq1pxd1btsv4g4pz9ikhmfgkfqngjcuo_y08fyrq_lkf9ny2iubxwy"
                        )
                    )
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
      cnvMessageTimer = Just (Ms {ms = 7684287430983198}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})
    }

testObject_Conversation_user_13 :: Conversation
testObject_Conversation_user_13 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))) (Domain "golden.example.com"),
      cnvType = RegularConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
      cnvAccess = [PrivateAccess, PrivateAccess, LinkAccess, LinkAccess, InviteAccess, CodeAccess, InviteAccess],
      cnvAccessRole = ActivatedAccessRole,
      cnvName = Just "\1059925\120234",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
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
                            "pj33g65zp3y35dnme_vedta8j2a3lx85z7m1isi_e87c3dztjm4_1duhtzn1fpkahqnwsdjwk50xqgawspoedhxkxld2bxmgyk9ghhz310hjtgy676sb0zbujo3"
                        )
                    )
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
                    omService = Nothing,
                    omConvRoleName =
                      (fromJust (parseRoleName "8evjw_y7w0w2l8qxaxr60chk7hd6hj98_mt3ing6xnwnpdca0qp42tomkmlci_jz4"))
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "nyucucx8i_emmsvmhhpfn3o8h9zidow5qn3hu60jnocrhi_9llcgqo5gc396rxdz6lpkct73l9h2bnfkyqyo1lpo1ga283fn2mqel3lrfopztj64siuzcxtl"
                          )
                      )
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))),
      cnvMessageTimer = Just (Ms {ms = 4379292253035264}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})
    }

testObject_Conversation_user_14 :: Conversation
testObject_Conversation_user_14 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))) (Domain "golden.example.com"),
      cnvType = RegularConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))),
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
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                          }
                      ),
                  memOtrMuted = False,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                  memOtrMutedRef = Nothing,
                  memOtrArchived = False,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    ( fromJust
                        ( parseRoleName
                            "pil4htbefn8i9yvwdkfam83c9gb70k1n3zkn_qb9esx177lofhgcv26no2u97l5uasehqgbb5rc36k46uf"
                        )
                    )
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))),
      cnvMessageTimer = Just (Ms {ms = 3200162608204982}),
      cnvReceiptMode = Nothing
    }

testObject_Conversation_user_15 :: Conversation
testObject_Conversation_user_15 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))) (Domain "golden.example.com"),
      cnvType = SelfConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))),
      cnvAccess = [PrivateAccess, PrivateAccess, InviteAccess],
      cnvAccessRole = PrivateAccessRole,
      cnvName = Nothing,
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                  memService = Nothing,
                  memOtrMuted = False,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
                  memOtrMutedRef = Just "",
                  memOtrArchived = False,
                  memOtrArchivedRef = Just "",
                  memHidden = True,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    ( fromJust
                        ( parseRoleName
                            "h2fasn_kwjucmy4spspzb6bhgimevoxevulwux13m3odd1clvy_okzb3rqpk9jg07z21fyquztzdrwpa2xa"
                        )
                    )
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) domain,
                    omService = Nothing,
                    omConvRoleName = (fromJust (parseRoleName "vztm5yqke"))
                  }
              ]
          },
      cnvTeam = Nothing,
      cnvMessageTimer = Just (Ms {ms = 6620100302029733}),
      cnvReceiptMode = Nothing
    }

testObject_Conversation_user_16 :: Conversation
testObject_Conversation_user_16 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))) (Domain "golden.example.com"),
      cnvType = One2OneConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))),
      cnvAccess = [InviteAccess, LinkAccess, LinkAccess],
      cnvAccessRole = TeamAccessRole,
      cnvName = Just "\USv",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
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
                    ( fromJust
                        ( parseRoleName
                            "6e8c9xxescuwzsvmczd844iza6d6xkkxklsgv52b9aj03a0_bkatzwfjsvtz313d6judvbpl0dlgswr2_nrd7h2hpw0_veg"
                        )
                    )
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                            }
                        ),
                    omConvRoleName = (fromJust (parseRoleName "amfmp778lcab6emu_l7z3ofb5lkbc1pvksfa9o226g9"))
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))),
      cnvMessageTimer = Just (Ms {ms = 3688870907729890}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})
    }

testObject_Conversation_user_17 :: Conversation
testObject_Conversation_user_17 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))) (Domain "golden.example.com"),
      cnvType = ConnectConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))),
      cnvAccess = [LinkAccess, LinkAccess],
      cnvAccessRole = ActivatedAccessRole,
      cnvName = Nothing,
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
                          }
                      ),
                  memOtrMuted = True,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                  memOtrMutedRef = Just "",
                  memOtrArchived = False,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Nothing,
                  memConvRoleName = (fromJust (parseRoleName "km3i"))
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
      cnvMessageTimer = Just (Ms {ms = 5675065539284805}),
      cnvReceiptMode = Nothing
    }

testObject_Conversation_user_18 :: Conversation
testObject_Conversation_user_18 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))) (Domain "golden.example.com"),
      cnvType = One2OneConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
      cnvAccess = [PrivateAccess],
      cnvAccessRole = ActivatedAccessRole,
      cnvName = Just "",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                  memService = Nothing,
                  memOtrMuted = True,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
                  memOtrMutedRef = Just "",
                  memOtrArchived = True,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    (fromJust (parseRoleName "u5k_f768_gbc0efd76xdd25k9xad2p4mxit0gpn4ihbp6iukqherpt3hop841_"))
                },
            cmOthers =
              [ OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                            }
                        ),
                    omConvRoleName = (fromJust (parseRoleName "8eai0sl3c2b0ude_hcp1ntoli4didzqbff"))
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                            }
                        ),
                    omConvRoleName =
                      ( fromJust
                          ( parseRoleName
                              "fbksch480c5wfn2d64n7mpjjiohdbpzpudtr4fkx8xknon122tia9kspnni_j0d53nx44nos47ms4l7v1v5c8srvc5v2"
                          )
                      )
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                            }
                        ),
                    omConvRoleName = (fromJust (parseRoleName "qft4gqk2wm7fcd7vsmnl9hsmo7izfqp7cnn_9mh6i9dme"))
                  },
                OtherMember
                  { omQualifiedId =
                      Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
                    omService =
                      Just
                        ( ServiceRef
                            { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                              _serviceRefProvider =
                                (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
                            }
                        ),
                    omConvRoleName = (fromJust (parseRoleName "9z5plhmkixcljnsfq4"))
                  }
              ]
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))),
      cnvMessageTimer = Nothing,
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -2})
    }

testObject_Conversation_user_19 :: Conversation
testObject_Conversation_user_19 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))) (Domain "golden.example.com"),
      cnvType = SelfConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))),
      cnvAccess = [LinkAccess],
      cnvAccessRole = TeamAccessRole,
      cnvName = Just "",
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                  memService =
                    Just
                      ( ServiceRef
                          { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                            _serviceRefProvider =
                              (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
                          }
                      ),
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
                            "2x93m15qf1a7el4t7sl_nob1q5q7urc7bb71l816331ktafgxukqlf1oc2b10e9w_5y724upn8kpzdfnpto1__keuuh217g0z1kq32v0w24hjus6s3tdxz1"
                        )
                    )
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
      cnvMessageTimer = Just (Ms {ms = 8984227582637931}),
      cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 2})
    }

testObject_Conversation_user_20 :: Conversation
testObject_Conversation_user_20 =
  Conversation
    { cnvQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))) (Domain "golden.example.com"),
      cnvType = ConnectConv,
      cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))),
      cnvAccess = [],
      cnvAccessRole = TeamAccessRole,
      cnvName = Nothing,
      cnvMembers =
        ConvMembers
          { cmSelf =
              Member
                { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                  memService = Nothing,
                  memOtrMuted = False,
                  memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
                  memOtrMutedRef = Just "",
                  memOtrArchived = True,
                  memOtrArchivedRef = Just "",
                  memHidden = False,
                  memHiddenRef = Just "",
                  memConvRoleName =
                    ( fromJust
                        ( parseRoleName
                            "0z8ynysylxkk56hg4kp26scdeuzyegfhcyzeroujq9vnm1pauclleesi3ql5f_zre59otqxymh6ege8p7d313djsxz3b78177ok1bx7k_gvll923r57a0"
                        )
                    )
                },
            cmOthers = []
          },
      cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))),
      cnvMessageTimer = Just (Ms {ms = 5214522805392567}),
      cnvReceiptMode = Nothing
    }
