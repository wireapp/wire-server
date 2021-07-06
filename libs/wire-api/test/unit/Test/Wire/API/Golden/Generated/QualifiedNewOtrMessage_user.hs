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
module Test.Wire.API.Golden.Generated.QualifiedNewOtrMessage_user where

import Data.Domain
import Data.Id (ClientId(..), Id(..))
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.UUID as UUID
import Imports
import Wire.API.Message
import Wire.API.User.Client

testObject_QualifiedNewOtrMessage_user_1 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_1 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "2"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ ( Domain "2w.h0cp9f711d.kw7ar.88.932.y"
                      , Map.fromList
                          [ ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), Map.fromList [])
                          ])
                    , ( Domain "970-7.b1fi9u9-z"
                      , Map.fromList
                          [ ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), Map.fromList [])
                          ])
                    ]))
          }
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = ""
    , qualifiedNewOtrClientMismatchStrategy =
        MismatchIgnoreOnly
          (Set.fromList
             [ Qualified
                 ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))))
                 (Domain "2rf-2u.c0f")
             , Qualified
                 ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))))
                 (Domain "qu.mk7.p5-bsom")
             , Qualified
                 ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))))
                 (Domain "340s7.h09.5.1.2a.k.w0.l3.t3d.r8")
             ])
    }

testObject_QualifiedNewOtrMessage_user_2 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_2 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "7"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList []))}
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Just LowPriority
    , qualifiedNewOtrData = ""
    , qualifiedNewOtrClientMismatchStrategy = MismatchReportAll
    }

testObject_QualifiedNewOtrMessage_user_3 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_3 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "5"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList []))}
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Nothing
    , qualifiedNewOtrData = "#\DC3\133"
    , qualifiedNewOtrClientMismatchStrategy = MismatchReportAll
    }

testObject_QualifiedNewOtrMessage_user_4 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_4 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "7"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList []))}
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = "1i\213"
    , qualifiedNewOtrClientMismatchStrategy = MismatchIgnoreAll
    }

testObject_QualifiedNewOtrMessage_user_5 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_5 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "0"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ ( Domain "74.w-5v9"
                      , Map.fromList
                          [ ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000004")))
                            , Map.fromList [(ClientId {client = "7"}, "\229")])
                          ])
                    ]))
          }
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = "v)"
    , qualifiedNewOtrClientMismatchStrategy =
        MismatchReportOnly
          (Set.fromList
             [ Qualified
                 ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))))
                 (Domain "798.c.c-6idf.r88-8i4-7.j")
             , Qualified ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002")))) (Domain "y9.pt")
             ])
    }

testObject_QualifiedNewOtrMessage_user_6 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_6 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "3"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList []))}
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = "<yQ"
    , qualifiedNewOtrClientMismatchStrategy = MismatchReportOnly (Set.fromList [])
    }

testObject_QualifiedNewOtrMessage_user_7 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_7 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "1"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ ( Domain "9mns-s.ww-ph5oy"
                      , Map.fromList
                          [ ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001")))
                            , Map.fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])
                          , ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), Map.fromList [])
                          ])
                    ]))
          }
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = "\234\SYN\171"
    , qualifiedNewOtrClientMismatchStrategy = MismatchReportAll
    }

testObject_QualifiedNewOtrMessage_user_8 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_8 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "4"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList [(Domain "cg7t2.rf-2", Map.fromList [])]))}
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just LowPriority
    , qualifiedNewOtrData = ""
    , qualifiedNewOtrClientMismatchStrategy = MismatchIgnoreAll
    }

testObject_QualifiedNewOtrMessage_user_9 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_9 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "5"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ (Domain "01l.s", Map.fromList [])
                    , (Domain "09v.uk", Map.fromList [])
                    , (Domain "2-28q.yda9.m", Map.fromList [])
                    , (Domain "95.pr8", Map.fromList [])
                    , (Domain "b0.l03.821--s5.0t.q7ky62t84r-0", Map.fromList [])
                    , (Domain "q.gy-7.w60zk24.n05", Map.fromList [])
                    ]))
          }
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = "\189u"
    , qualifiedNewOtrClientMismatchStrategy = MismatchIgnoreOnly (Set.fromList [])
    }

testObject_QualifiedNewOtrMessage_user_10 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_10 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "8"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ (Domain "79-y-r4-9.d", Map.fromList [])
                    , (Domain "7f3.ra.9.r37.xavdz88-9vw-z", Map.fromList [])
                    , (Domain "7g.hw9aq-1", Map.fromList [])
                    , (Domain "8w5.g5l-7.tys", Map.fromList [])
                    , (Domain "n.659-s.nfd", Map.fromList [])
                    , (Domain "pc5s-p9-48-x.r8cq.ss89h", Map.fromList [])
                    ]))
          }
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Nothing
    , qualifiedNewOtrData = "GL\ACK"
    , qualifiedNewOtrClientMismatchStrategy = MismatchIgnoreAll
    }

testObject_QualifiedNewOtrMessage_user_11 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_11 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "7"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList []))}
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just LowPriority
    , qualifiedNewOtrData = ""
    , qualifiedNewOtrClientMismatchStrategy =
        MismatchIgnoreOnly
          (Set.fromList
             [ Qualified
                 ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))))
                 (Domain "a9z----8fk.aa96.rd67.c-gtj63")
             , Qualified
                 ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))))
                 (Domain "315-o1n.i7sp.s9o")
             ])
    }

testObject_QualifiedNewOtrMessage_user_12 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_12 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "6"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList []))}
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just LowPriority
    , qualifiedNewOtrData = "\192\162\201"
    , qualifiedNewOtrClientMismatchStrategy =
        MismatchReportOnly
          (Set.fromList
             [ Qualified
                 ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))))
                 (Domain "042.d-3.gs-6")
             , Qualified
                 ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))))
                 (Domain "820ea4z19oe.10vbh.gmu7.s78s")
             , Qualified ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002")))) (Domain "x3q.gd")
             ])
    }

testObject_QualifiedNewOtrMessage_user_13 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_13 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "8"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ ( Domain "0i7268.w"
                      , Map.fromList
                          [((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), Map.fromList [])])
                    , ( Domain "l6.3.cf9d-z9wd.ay4qmup"
                      , Map.fromList
                          [((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), Map.fromList [])])
                    , ( Domain "o.ecc22-8z0s"
                      , Map.fromList
                          [((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), Map.fromList [])])
                    ]))
          }
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Just LowPriority
    , qualifiedNewOtrData = " "
    , qualifiedNewOtrClientMismatchStrategy = MismatchReportOnly (Set.fromList [])
    }

testObject_QualifiedNewOtrMessage_user_14 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_14 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "0"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList []))}
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Nothing
    , qualifiedNewOtrData = "zC"
    , qualifiedNewOtrClientMismatchStrategy = MismatchIgnoreOnly (Set.fromList [])
    }

testObject_QualifiedNewOtrMessage_user_15 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_15 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "2"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ ( Domain "32-d.o0t410.de1kn"
                      , Map.fromList
                          [ ( (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000002")))
                            , Map.fromList [(ClientId {client = "1"}, "")])
                          ])
                    ]))
          }
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just LowPriority
    , qualifiedNewOtrData = "@\246a"
    , qualifiedNewOtrClientMismatchStrategy = MismatchIgnoreAll
    }

testObject_QualifiedNewOtrMessage_user_16 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_16 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "1"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ ( Domain "skmj4.u5471dp4v"
                      , Map.fromList
                          [ ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), Map.fromList [])
                          , ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), Map.fromList [])
                          ])
                    ]))
          }
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = "\149q"
    , qualifiedNewOtrClientMismatchStrategy = MismatchReportAll
    }

testObject_QualifiedNewOtrMessage_user_17 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_17 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "8"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients {qualifiedOtrRecipientsMap = (QualifiedUserClientMap (Map.fromList []))}
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = "\r\185"
    , qualifiedNewOtrClientMismatchStrategy =
        MismatchReportOnly
          (Set.fromList
             [ Qualified
                 ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))))
                 (Domain "142--h.4f.h8")
             ])
    }

testObject_QualifiedNewOtrMessage_user_18 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_18 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "7"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ (Domain "28d6.q1s.e", Map.fromList [])
                    , (Domain "8.z7q.pfl", Map.fromList [])
                    , (Domain "9ihzlr.16.74.kbz-8p.46.2i-2qd897-lx.f-4", Map.fromList [])
                    , (Domain "c8v77-j.q5l1-u-y9h.0a.8.14.a", Map.fromList [])
                    , (Domain "d2.rt", Map.fromList [])
                    , (Domain "k6u.s.suf", Map.fromList [])
                    , (Domain "kohn-bnoq0.vqi", Map.fromList [])
                    , (Domain "y2-4i9.zq--dg", Map.fromList [])
                    ]))
          }
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Just HighPriority
    , qualifiedNewOtrData = "5A"
    , qualifiedNewOtrClientMismatchStrategy = MismatchReportAll
    }

testObject_QualifiedNewOtrMessage_user_19 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_19 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "0"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ (Domain "54f.hc-82g.xjvk26y5v7.1.qc", Map.fromList [])
                    , ( Domain "8hhm.a-77q6"
                      , Map.fromList
                          [ ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000")))
                            , Map.fromList [(ClientId {client = "0"}, ""), (ClientId {client = "1"}, "")])
                          ])
                    ]))
          }
    , qualifiedNewOtrNativePush = True
    , qualifiedNewOtrTransient = True
    , qualifiedNewOtrNativePriority = Nothing
    , qualifiedNewOtrData = "\237i'"
    , qualifiedNewOtrClientMismatchStrategy =
        MismatchReportOnly
          (Set.fromList
             [ Qualified
                 ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))))
                 (Domain "u--o0nc.e5.d-31i6")
             ])
    }

testObject_QualifiedNewOtrMessage_user_20 :: QualifiedNewOtrMessage
testObject_QualifiedNewOtrMessage_user_20 =
  QualifiedNewOtrMessage
    { qualifiedNewOtrSender = ClientId {client = "5"}
    , qualifiedNewOtrRecipients =
        QualifiedOtrRecipients
          { qualifiedOtrRecipientsMap =
              (QualifiedUserClientMap
                 (Map.fromList
                    [ (Domain "0-89.1l.ls", Map.fromList [])
                    , (Domain "0x2.svo", Map.fromList [])
                    , (Domain "3.gq0--73uhk1n", Map.fromList [])
                    , (Domain "7.w95.uj", Map.fromList [])
                    , (Domain "h-o3.fi", Map.fromList [])
                    , (Domain "j3.h2xmcb", Map.fromList [])
                    , (Domain "l.pkx62-e-o2", Map.fromList [])
                    , (Domain "oj.e", Map.fromList [])
                    , (Domain "wh4.f-x3.fiqqx.zm-3q", Map.fromList [])
                    , (Domain "y5.f6", Map.fromList [])
                    , (Domain "yi5c.i-p", Map.fromList [])
                    ]))
          }
    , qualifiedNewOtrNativePush = False
    , qualifiedNewOtrTransient = False
    , qualifiedNewOtrNativePriority = Just LowPriority
    , qualifiedNewOtrData = " "
    , qualifiedNewOtrClientMismatchStrategy = MismatchReportAll
    }
