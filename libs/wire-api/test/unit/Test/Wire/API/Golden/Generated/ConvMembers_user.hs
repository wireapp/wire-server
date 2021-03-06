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
module Test.Wire.API.Golden.Generated.ConvMembers_user where

import Data.Domain
import Data.Id (Id (Id))
import Data.Qualified
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation
  ( ConvMembers (..),
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
    OtherMember (..),
  )
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_ConvMembers_user_1 :: ConvMembers
testObject_ConvMembers_user_1 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                    }
                ),
            memOtrMuted = True,
            memOtrMutedStatus = Nothing,
            memOtrMutedRef = Just "",
            memOtrArchived = True,
            memOtrArchivedRef = Just "",
            memHidden = False,
            memHiddenRef = Just "2",
            memConvRoleName =
              ( fromJust
                  ( parseRoleName
                      "pqzher6cs67kz8fg0cd4o8aqs00kvkytkovzkjs1igz9eub_5xey_no8m2me3or8ukbtv05uq7gc54p6g52kwiygyqs3om7yu0istkixp_3395mkaxh9zljjyy8"
                  )
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    (parseRoleName "y4zf98vsd7b6zi1_3wch87_k8m0t8mpdhh8zlcq461s80oc0sl7yn85twxn89f7f4kwpd4_hj9q2m3za")
                )
            }
        ]
    }

testObject_ConvMembers_user_2 :: ConvMembers
testObject_ConvMembers_user_2 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Nothing,
            memOtrMutedRef = Just "",
            memOtrArchived = False,
            memOtrArchivedRef = Nothing,
            memHidden = False,
            memHiddenRef = Just "",
            memConvRoleName = (fromJust (parseRoleName "hoz2iwweprpt270t14yq_ge8dbej"))
          },
      cmOthers = []
    }

testObject_ConvMembers_user_3 :: ConvMembers
testObject_ConvMembers_user_3 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Nothing,
            memOtrMutedRef = Just "M",
            memOtrArchived = True,
            memOtrArchivedRef = Just "5",
            memHidden = False,
            memHiddenRef = Nothing,
            memConvRoleName = (fromJust (parseRoleName "l3e1jootef7"))
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "_iat9eeo3d3hpegxoagnv_edxygxnt22l8x018dcrvdn1yldv"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "24cmwkzx__1kpkialogtzp4709ii9aa1_j91lxewed0jl15bsoka50u44_m2yp2tn5jcyk353rlj17a4lfs5mu9psf2mrz484mr38t_w4uiemk"
                    )
                )
            }
        ]
    }

testObject_ConvMembers_user_4 :: ConvMembers
testObject_ConvMembers_user_4 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                    }
                ),
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
                      "h7lmyguc3tspi_iq2rb96dnujrrnumratq1xtq6aj15x3uzme6jptzvww78_u22iakspdllhuwsap36t4m2j2cui6cjqciv5b_qqfql2r2rrevw9saof6q77d"
                  )
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) domain,
              omService = Nothing,
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "8pab2pgrjaf085srseyhvavmz9nveubwyrgh4788ilfb0rj2t3gh9izi6bkk97io06aj0bwv1868hzpufqnf_pdilz4ho"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
                      }
                  ),
              omConvRoleName =
                (fromJust (parseRoleName "mhxmfwnaxzygxlgoomh4jg1l2pttlem3seyve7cqheq7n8rtdyv2ovxfi7x84"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "jx92xe1j6i9wkc9na5um7bca1m94at3gjoc81nn09667vg9xt32zlkldec05cumwltcyxwzj2sj089cdzu0iqso5br2nuk7s67je6xj8i9g8h5tmhzuosqq4wktmmf"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
                      }
                  ),
              omConvRoleName =
                (fromJust (parseRoleName "3lwoqfe65zjajm279ixflg1es4vbo8u004reefel78tgyo231qj968zo9bhr3"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "8178_"))
            }
        ]
    }

testObject_ConvMembers_user_5 :: ConvMembers
testObject_ConvMembers_user_5 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                    }
                ),
            memOtrMuted = True,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
            memOtrMutedRef = Just "",
            memOtrArchived = False,
            memOtrArchivedRef = Just "2",
            memHidden = True,
            memHiddenRef = Just "\DC1",
            memConvRoleName =
              ( fromJust
                  (parseRoleName "v1y6zn1esg9ptv72rpu4speujl6uqick58m5bsbuqjdg9xs83ei1hvryvsyhvn7o1zntculg6adry1gvglpe")
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "j6cd868soxwsiwf4iaynv371tiotfd9kio"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "h9e303sddeozp391xuq19j0xt6dc26"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "w0f6se19tlklqr46v0i1c5t5ii4dc123"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "xew1uato1o"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
              omService = Nothing,
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "zz07p8aovj0wfrztrwe7b4u02ysxeg72oyfo8dnqzw7odzl7iym1tdsok27d76po1t8b3n9fsin0r8543ruheaoylie99aqjjeiz5ce2wjmdcb4nro2b1pb7p"
                    )
                )
            }
        ]
    }

testObject_ConvMembers_user_6 :: ConvMembers
testObject_ConvMembers_user_6 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                    }
                ),
            memOtrMuted = True,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
            memOtrMutedRef = Just "",
            memOtrArchived = False,
            memOtrArchivedRef = Just "",
            memHidden = False,
            memHiddenRef = Just "T",
            memConvRoleName =
              ( fromJust
                  ( parseRoleName
                      "tyugtdx7b6_tnvz0btwo03zht0ee07jgmjgn5cbi6vxf8ge3dte6s2_hz0owrju_hx5g14wpkpr_9wdr_lepejkncj"
                  )
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    (parseRoleName "w794tuc4_7uds0z1njq0n64rfwgh1ejkzozuikjk845ybh9r4l3d6y1o_v1qo108ri4yhnbpetkhzy1ie95")
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "skl0wbt9"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "2u7h4liwioxhdyw2ums20wc4uuu078x416195m0"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "v7lfuggvm03jjo87bwn1kdwdt86rj7x75gp4t8e8iyii6_rcotf6ojkaczcs93ydllwfn4fybjasv4ediol0auyb7_l2omz74k8iw7lkm4l622v5i2qbqn2e"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "s53zd8pxzf5jnywo11tcqhus0v86oo93vgffrnfao7zv7ddvutnwzs1sv_zoh1nxeqanlyj29x2crpuhrged_hn40pdiefm"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "tn14gkbhny6tynh1ijhruy_nsvw17_wnrd19sbcffh9xmkrpilwfc9wnbv"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "xlaoiqbhmdxeak4vygfx2yw9y1dwxlv2u3_80yrdtu5i5wtkk1y"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "by4w8spnxlbgatis64iz_"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    (parseRoleName "20ki2zvvhvpe4jlvdvvbpzxlfx3nije8ionba35ovbcdrte4y7pjws17jz3bhct7dy20x_8k9sxo34smx25")
                )
            }
        ]
    }

testObject_ConvMembers_user_7 :: ConvMembers
testObject_ConvMembers_user_7 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
            memOtrMutedRef = Just "\RS",
            memOtrArchived = True,
            memOtrArchivedRef = Just "\54519",
            memHidden = False,
            memHiddenRef = Just "",
            memConvRoleName =
              ( fromJust
                  ( parseRoleName
                      "lqx9nuwumdniap26x6xql_bqj63xsg345w41xmlgddcdcalubn3yz8xddzyw7q0447yy5xs9g_s3lyfvq7vbmgl25up11z9yt"
                  )
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    (parseRoleName "gslt5tuoes3wnlf6td917g650hvja7d9lsl7imebq8hy6he50xoz498jxu37m2kdm98egyblf6e9jk8k71gj")
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "vb3ptbhwr5pdikr5avb56bsc2qmlbvmxr_bjry6j1veyz4ppilanfkzq2bv"))
            }
        ]
    }

testObject_ConvMembers_user_8 :: ConvMembers
testObject_ConvMembers_user_8 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Nothing,
            memOtrMutedRef = Just "",
            memOtrArchived = True,
            memOtrArchivedRef = Just "",
            memHidden = True,
            memHiddenRef = Just "?",
            memConvRoleName = (fromJust (parseRoleName "4mpg6odm53b6afqtdk1wwr57dsoa1jiwhne"))
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
              omService = Nothing,
              omConvRoleName =
                (fromJust (parseRoleName "vcat_x7n_17ig7gxw1mjyk912pjh8furzbhzfxpo0citjipmkf0cy3j9sqekrxcn1su7fs"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "m_7vhmo6z7kwi0mu1qh9d1o0ztn3t9u1l1gv62bwcbawq89bgy"))
            }
        ]
    }

testObject_ConvMembers_user_9 :: ConvMembers
testObject_ConvMembers_user_9 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
            memOtrMutedRef = Just "",
            memOtrArchived = True,
            memOtrArchivedRef = Just "",
            memHidden = True,
            memHiddenRef = Just "I",
            memConvRoleName = (fromJust (parseRoleName "pi6noe34tsfw6"))
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
              omService = Nothing,
              omConvRoleName =
                (fromJust (parseRoleName "ytko3vik7nviewv_mlrluswwsoxkxdwexmdy1r7yy1l265cdg5nluqedrxby2zma"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "pwsqgpb966m2o9g9oahmfp83gjb0ll987xdvus_bdxgo3p0gokb6spardga87x__5ueyjpgvy_6lzhimz2_1d967fpvfbs236x6ed777nf0mfuw4j5x4wtk"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "8mveq505d0ftsgaefuyu"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "tr_"))
            }
        ]
    }

testObject_ConvMembers_user_10 :: ConvMembers
testObject_ConvMembers_user_10 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
            memService = Nothing,
            memOtrMuted = False,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
            memOtrMutedRef = Just "",
            memOtrArchived = True,
            memOtrArchivedRef = Just "\1089705",
            memHidden = False,
            memHiddenRef = Just "",
            memConvRoleName =
              ( fromJust
                  (parseRoleName "7dbiql5aifhtio8krbpps8i1r20uplkfvmii9o7nem8r2xe9jyh4vqgw_dt8dznua4ms_ojuy7x8mhko5")
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "svx4t8g9wwsxnjh"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "ozu_7m73jbvhbk0i3jcoyoe4dh2tuew"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                      }
                  ),
              omConvRoleName =
                (fromJust (parseRoleName "tm8gb5smre1aboo4262eckaqv0imy0ke4kzbi9l8eq0yem6g6jjqcx6uavcsuavc"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "50w5dsrib7kvjl_i3w_w8wx0kiilysbj_mdy0hycpcico768nytkqi0d"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "t6f29t4sp_bg9v3_6g2inptndogq1yya8hena72erf5pwi5o47k9hp3x_n4wusj9gfwnhn7v419ovmug4ttvghtlfvyzqgls2wcj_"
                    )
                )
            }
        ]
    }

testObject_ConvMembers_user_11 :: ConvMembers
testObject_ConvMembers_user_11 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
                    }
                ),
            memOtrMuted = True,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}),
            memOtrMutedRef = Just "",
            memOtrArchived = False,
            memOtrArchivedRef = Just "\DC1",
            memHidden = False,
            memHiddenRef = Nothing,
            memConvRoleName = (fromJust (parseRoleName "7gd37ta2eg_"))
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "w9hqf8lcwwex77nxrgqaro7a97i0up89j1xsz9zv65vvy2vbshfue_bk6h0dqsfcom4yhiy_eusbj84s8hi33rps5lxr"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "bkf39px76nwbf4mz6_b8wuqltfv5dgiwp75ji5bh7tfjrl37zcchv"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "4_9pqyp2h9wt3g2ek62u5zxfs7njrdsbbas9_pnigxjyru5wbbd8rvd1"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "x8aoqx69ptpetu2ijqtdgfcnn4i_fs53xuuvd8wk0w62az2ifcf3"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    (parseRoleName "3x2x2c653mxqbc3033be6nw33o4kbz59c_m2840prcxccsih0vu_xiiij4p2bcuapi2a5pdah43f0cdcw")
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "5mxf8j2rudyj_qp34xght54oi5ze5ibz9gdn0r4c3bswshmh24289g8sexu90fxpbi2in3b0fnim_zkyh7w5jzwl_rr4pxd35sqq4spql3ky60zdoobj"
                    )
                )
            }
        ]
    }

testObject_ConvMembers_user_12 :: ConvMembers
testObject_ConvMembers_user_12 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
            memOtrMutedRef = Nothing,
            memOtrArchived = True,
            memOtrArchivedRef = Just "\157525",
            memHidden = False,
            memHiddenRef = Just "?",
            memConvRoleName =
              (fromJust (parseRoleName "metu5hokrekxgyamdqvu5pub1efki_5x00gpjigwjuj5wmyfi6ipy425i87a3phcq"))
          },
      cmOthers = []
    }

testObject_ConvMembers_user_13 :: ConvMembers
testObject_ConvMembers_user_13 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                    }
                ),
            memOtrMuted = True,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
            memOtrMutedRef = Just "\v",
            memOtrArchived = False,
            memOtrArchivedRef = Just "",
            memHidden = True,
            memHiddenRef = Just "\DC1",
            memConvRoleName =
              (fromJust (parseRoleName "42ujl2m6nrjp4ieej3a79t55_q2aqubgpihl2e1q3pv98wtyqxhaebz2n_rnzc95wyzurylgh8ju5g"))
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "ucf8dtd6st2z1dpc4vty0rkep2m2"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                      }
                  ),
              omConvRoleName =
                (fromJust (parseRoleName "z9plklhipd9vc9xsrmmnjgxvcmn88wd8bsxksz4fz0m_vxochm6fknmxlhv8q7t6pz7zem"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "p5hsxn0iq7cuh0m49o1ra9x3kji9ku088q"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "a7nlm4pb71iu7auorq2wfdwvbqjamumby1l8o_0t_mrrdq8ucurulsrl8j35ulczgcw4h9vli187z74gh2ibutc5jt6esfil9m9t"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "tjzby42utlpbmouqok_0l9zhoqba03bfz_7hgmuk2r1gcd5laozwgu6zwactg8okxg07url_2huwyn1w_5yl9e0dqq_oweplfx3fkg6c365949zbi86c9tsfyf35h0o"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
              omService = Nothing,
              omConvRoleName =
                (fromJust (parseRoleName "pk30v1kj11lzl1l3dty1i5eucmceer6p41wwq5hlewecwzxn_s5sv8lpwmvr7b5_b"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
              omService = Nothing,
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "mh1oif358ntz8xlflodiay7alkiktphvvkfy0jerup_gryt4gm0p08q7ynx1kak7de0uup8o9rdblv118f9fal2c8flmub8vdgt1u8nyphxoj2kq17y"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "y000q2czyxtw7ix6b0cma"))
            }
        ]
    }

testObject_ConvMembers_user_14 :: ConvMembers
testObject_ConvMembers_user_14 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Nothing,
            memOtrMutedRef = Nothing,
            memOtrArchived = True,
            memOtrArchivedRef = Just "_",
            memHidden = True,
            memHiddenRef = Nothing,
            memConvRoleName =
              ( fromJust
                  ( parseRoleName
                      "dzc663lqhsl5koo9zoydav2svibxi2rldgfs21lozv_47live4tdb_6etua7xbo2h08iehbijx6cnvaiamahoh304_6cua4cce27n8uf1y"
                  )
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "t7iw7icbnz53p8to8m6b0qjgp8aqd1w"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "dpw7b81s21xnmn230l3meyc9xb50h44adxfjrlcwc4rvu9y46whvv_9_06gr1o8mxqa6fwmhvzq8ugirkige_o440kgu1xzncja856hbiy0wosrfbkohxlnt3ad0n1nf"
                    )
                )
            }
        ]
    }

testObject_ConvMembers_user_15 :: ConvMembers
testObject_ConvMembers_user_15 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                    }
                ),
            memOtrMuted = True,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
            memOtrMutedRef = Just "y",
            memOtrArchived = True,
            memOtrArchivedRef = Just "",
            memHidden = True,
            memHiddenRef = Just "\1007867",
            memConvRoleName = (fromJust (parseRoleName "gtot_tiqu6niipivhw02eu"))
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
              omService = Nothing,
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "q2bss_quf29jonuixms3y4xvohcq387gmgft5imwjk04u1skbjjvg6h92jgfk00j9g5meownkwzijda98xkdzs6"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "jmp56f7fu12_0ufmn4ykdhtdqwxy3atmutw7hkix8ed6"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "razn_c23lmdd_fpf41hto60aw5iftmyrwna_1o"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "e5zwjlctzwwxuks0hxcl8an6oas840jrs7xl2gw1p8rlnun2_rtfa_rw9tsb67x3g4dh4cv62tvx10x4bmmj0dzczsyb_ic0gvzhvsm1vo"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) domain,
              omService = Nothing,
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "6bsf2pcmtt15vznyahwm_hsz4fvc94fd9l3etor9h5eok2ggp7gmy8512428lrq1sezdb9xqz8v33ooyr7vyc39x8kohh0cxc_dja4wqgqx_59xyxqbah94tsyx"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "7t50u1ug50o9xo49dbv0g46wbhgu17_4pe6hsooiqye50_a0s0p7i78lwx"))
            }
        ]
    }

testObject_ConvMembers_user_16 :: ConvMembers
testObject_ConvMembers_user_16 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
                    }
                ),
            memOtrMuted = True,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
            memOtrMutedRef = Nothing,
            memOtrArchived = True,
            memOtrArchivedRef = Nothing,
            memHidden = True,
            memHiddenRef = Just "",
            memConvRoleName =
              ( fromJust
                  ( parseRoleName
                      "0cbihjuksvzpyno3h86i34yoy6hr2o2yg16_wi8247u84jontkapcs_cms98qq8qkuw9zoa240fdjf8vlyhfsxidoxy1t3k_wefqly5"
                  )
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
              omService = Nothing,
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "xswztznf4h445esgv862hlnl_w12ofs6gudt5rqm0fmcy1ji1j1tmdxud9knxuv6_1tkj9xdm8e6heqi2aa3nczuyzfckca"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    (parseRoleName "bdee121l0vor_rgs35m64b7akc1ulu2djh36kzp53faqyk2d366wzr56xk2ozzqy2gnq2it_rox4ir6")
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "iuv0fiqut43alvkb_1b3h7dw_nkxu7yco1lbwot_4ly_3r9ru8qudjl4u4rh1xqcmuxh8t6oob6e06br1kn9x6niubo13xwetasp3"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "c3cjc"))
            }
        ]
    }

testObject_ConvMembers_user_17 :: ConvMembers
testObject_ConvMembers_user_17 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
            memOtrMutedRef = Just "",
            memOtrArchived = True,
            memOtrArchivedRef = Just "",
            memHidden = False,
            memHiddenRef = Just "",
            memConvRoleName = (fromJust (parseRoleName "gfzbewc89276dh2bhpftvkqv1lfti5ai_n"))
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "rd9k2n2q8n7gbm55"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "yctlmr8wp0tiyiyn5qld86rpeqigr83h6hn99zo4x1s5ps5j5es4w00wvpcg9h3eplsvctvzryjle1ynou4f7h7s_z0awql5ownx1joi5var2tt0g"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "6s2wm62g83wg327vlylm0rv64e3yt41j9ofekhhybvwlxx6b9hwtst9sed3s6kh3dlkp61ocjbpxqt033xic98ujq__ck3yn"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "mjpdiejf9fgpxz"))
            }
        ]
    }

testObject_ConvMembers_user_18 :: ConvMembers
testObject_ConvMembers_user_18 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
                    }
                ),
            memOtrMuted = False,
            memOtrMutedStatus = Nothing,
            memOtrMutedRef = Just "",
            memOtrArchived = False,
            memOtrArchivedRef = Just "",
            memHidden = False,
            memHiddenRef = Nothing,
            memConvRoleName = (fromJust (parseRoleName "5crfexlvlxn_"))
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "zb1ddtg"))
            }
        ]
    }

testObject_ConvMembers_user_19 :: ConvMembers
testObject_ConvMembers_user_19 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
            memService =
              Just
                ( ServiceRef
                    { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                      _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                    }
                ),
            memOtrMuted = True,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
            memOtrMutedRef = Just "G",
            memOtrArchived = False,
            memOtrArchivedRef = Just "v",
            memHidden = True,
            memHiddenRef = Just "I",
            memConvRoleName =
              ( fromJust
                  ( parseRoleName
                      "t4grrg6rq1gacy62wjoc_h5ytk9o4_jv25gr1a6ycai9ylhuyis7m5s062y4e5iyz0xtw_rbmjuzvgdjkn0tzl9a23p_nycqtp7foswfpxgmwahe02iae1eih"
                  )
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000003"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "oqgwbsi97htc7jnxrlo"))
            }
        ]
    }

testObject_ConvMembers_user_20 :: ConvMembers
testObject_ConvMembers_user_20 =
  ConvMembers
    { cmSelf =
        Member
          { memId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
            memService = Nothing,
            memOtrMuted = True,
            memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}),
            memOtrMutedRef = Just "\18833",
            memOtrArchived = True,
            memOtrArchivedRef = Just "",
            memHidden = True,
            memHiddenRef = Just "*",
            memConvRoleName =
              ( fromJust
                  ( parseRoleName
                      "3esspudy3w9915yyyonlb3dzmp3blkjrvdfdjejfojfkrhosbtnhsivt6dhutihlq6ija_azgdrx6r4_pdw1a9cj0opv77g087t"
                  )
              )
          },
      cmOthers =
        [ OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "b_1s3qhqvz8buhk16hx5kz7moxy6w"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "k5dvtuimn88rwf702hfi3dyn3mmcp_4_rlqogl2t4v29vl5ynu9cnv4n4zi7lat5lwptqerygp5vbq9297sniahtqof3nrah_lotb9m5mowj1p8r049"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "qhzt3rgqtwdtenauy_ju9kov8ezecou02bo9y3sj4gl7thjyw7hvwmryfmnyzjx0r25zzdsi9f8kifueppwtcmlz8kwq0vj31j88ziu2p66o5quz2vt9w"
                    )
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
              omService = Nothing,
              omConvRoleName = (fromJust (parseRoleName "4fn4nrdzoy1wqumz7550ee6kof6om6e5vy2iii_91ejzcvb8h8rihehyz"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
                      }
                  ),
              omConvRoleName =
                ( fromJust
                    (parseRoleName "t7fwbdtuxk3vgdd_x_q5rpdpz6m_iirgo62f151bh1m191gtu6rirc3wrthfjz188k22cip05ds6k15klrl6")
                )
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
              omService =
                Just
                  ( ServiceRef
                      { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                        _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
                      }
                  ),
              omConvRoleName = (fromJust (parseRoleName "axoworkfvytprcph0ztay_w55o7ipzy7uj4x52evws9mfw987n30w3"))
            },
          OtherMember
            { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) domain,
              omService = Nothing,
              omConvRoleName =
                ( fromJust
                    ( parseRoleName
                        "bhkqaueueah3dgdxrjw8a_mvd2y1jatjhzz5nhx6smsy5dw2a9ru67mqdnx1cbwc8bl5_fupckro1vh5scexr9o1z7pyy4"
                    )
                )
            }
        ]
    }
