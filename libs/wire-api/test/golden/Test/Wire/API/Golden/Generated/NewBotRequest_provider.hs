{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.NewBotRequest_provider where

import Data.Domain
import Data.Handle (parseHandle)
import Data.ISO3166_CountryCodes
  ( CountryCode
      ( AO,
        AR,
        AT,
        CV,
        FI,
        GR,
        IO,
        JM,
        KH,
        KN,
        MD,
        ML,
        MO,
        MW,
        NP,
        SY,
        TD
      ),
  )
import Data.Id
import Data.LanguageCodes qualified
  ( ISO639_1
      ( AB,
        CO,
        CV,
        DZ,
        GU,
        HA,
        HI,
        LN,
        MI,
        NG,
        NR,
        NY,
        RW,
        SI,
        SK,
        SM,
        TA,
        TE,
        ZU
      ),
  )
import Data.Qualified
import Data.UUID qualified as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, (.))
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Locale
import Wire.API.Provider.Bot
  ( BotUserView
      ( BotUserView,
        botUserViewColour,
        botUserViewHandle,
        botUserViewId,
        botUserViewName,
        botUserViewTeam
      ),
    botConvView,
  )
import Wire.API.Provider.External (NewBotRequest (..))
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))
import Wire.API.User.Profile
  ( ColourId (ColourId, fromColourId),
    Name (Name, fromName),
  )

domain :: Domain
domain = Domain "golden.example.com"

testObject_NewBotRequest_provider_1 :: NewBotRequest
testObject_NewBotRequest_provider_1 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000000")),
      newBotClient = ClientId 0xc,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
            botUserViewName =
              Name
                { fromName =
                    "\SOHt\92170\187244\&7@}\GS+\RS8o<&\158394P\9460\DELv\23032\FS\SUB=\4996\62110\fla\181159\SI))\SOH\177427HX\1014166}\170602C9\r% Quy.\111144\98964\fp1S}R\95152]y'\NAKA\166125A\132567BI\GS\1092797\1022808V\34673e^\SO5b#\1060042$u#b+f\1083928s\170695[\985436}\185377K\bB'Ux/.\97028\139704x\179497\&3)\152359\DC3r4dl"
                },
            botUserViewColour = ColourId {fromColourId = 0},
            botUserViewHandle = Nothing,
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
          },
      newBotConv =
        botConvView
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
          (Just "")
          [ OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))
                        }
                    ),
                omConvRoleName =
                  fromJust (parseRoleName "nnu9fdovdb35gac26w1tou0uax_3b9l8y5sgh795f4d7yr1gzuewqfj8hx4")
              },
            OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
                omService = Nothing,
                omConvRoleName = fromJust (parseRoleName "3m_oredfy0jqp1jvrociab2vq4z1rzklzs6_bpd04ht0")
              },
            OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
                omService = Nothing,
                omConvRoleName =
                  fromJust
                    ( parseRoleName
                        "0ns0gbsu3sk2cj6qsbs8bkmmculfhcbp_wntqaciff2f3j0zwf24p2ga7lxkzd13c626ruj7evj1lyqn0u7m2q5su"
                    )
              }
          ],
      newBotToken = "&",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.TA, lCountry = Just (Country {fromCountry = CV})}
    }

testObject_NewBotRequest_provider_2 :: NewBotRequest
testObject_NewBotRequest_provider_2 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000003")),
      newBotClient = ClientId 4,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
            botUserViewName = Name {fromName = "}\DLE&:\bp\ETB.+H\59688 \RS\SYNq\1068740\37311"},
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle = Just (fromJust (parseHandle "mwt6")),
            botUserViewTeam = Nothing
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) Nothing [],
      newBotToken = "f\ACK",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.SI, lCountry = Just (Country {fromCountry = JM})}
    }

testObject_NewBotRequest_provider_3 :: NewBotRequest
testObject_NewBotRequest_provider_3 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000003-0000-0004-0000-000000000001")),
      newBotClient = ClientId 7,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
            botUserViewName =
              Name
                { fromName =
                    "T\146213nw)\1029633\&8-CL\1023591^Xu\1098665\149637\EMq7\v\SOH1\EM\CAN\95353\&3\26848\"\ACK(\153989\US`/\RS\b\1003810\\,\187310\SIV\839kg\3419/\1079339iZ\1072092;"
                },
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle =
              Just (fromJust (parseHandle "h.cn77ac0vrssl3li_xktkmwmps_8s6y-ntsnv5e6i6pc4tihqh6t9paxuyxopod76mgse-4pyop9v.n6uhz5")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
          },
      newBotConv =
        botConvView
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
          Nothing
          [ OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))
                        }
                    ),
                omConvRoleName =
                  fromJust
                    ( parseRoleName
                        "xawj0wsxkoiigr6hjuhzkt2qdrnx2hc3auf74uyekse8rrmrtv05sysqlhs9c2bq87h_pz5di6rjr8_bapds"
                    )
              }
          ],
      newBotToken = "0~",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.AB, lCountry = Just (Country {fromCountry = IO})}
    }

testObject_NewBotRequest_provider_4 :: NewBotRequest
testObject_NewBotRequest_provider_4 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000000")),
      newBotClient = ClientId 0xf,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
            botUserViewName =
              Name
                { fromName =
                    "/3\1027409s\166702\150783Hf\r.Z+l\ACK\11408j\ETB\\\98546@;`\vC\1079074.tt~\1007952\&4~d~fI\SUB*\179540"
                },
            botUserViewColour = ColourId {fromColourId = -1},
            botUserViewHandle = Nothing,
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) (Just "") [],
      newBotToken = "R",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.DZ, lCountry = Just (Country {fromCountry = MD})}
    }

testObject_NewBotRequest_provider_5 :: NewBotRequest
testObject_NewBotRequest_provider_5 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000001-0000-0002-0000-000300000003")),
      newBotClient = ClientId 4,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
            botUserViewName =
              Name
                { fromName =
                    "\ETXV\EM/S\58505\1052803vc\CAN\SIk/!\178433sEL\38828\31920\CAN\139357\41899\&3\1056807JWo\ESC=0YZHAl+\1064243\CAN)\125039W\59265"
                },
            botUserViewColour = ColourId {fromColourId = -1},
            botUserViewHandle =
              Just (fromJust (parseHandle "dcd5u---q-5liar3qaixbwwjjrg-79a2k413z74whfyc-k_8jvle63fhs3v.mdncia29")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) (Just "}") [],
      newBotToken = "\ESC\GS\SI",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.NG, lCountry = Nothing}
    }

testObject_NewBotRequest_provider_6 :: NewBotRequest
testObject_NewBotRequest_provider_6 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000003")),
      newBotClient = ClientId 2,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
            botUserViewName =
              Name
                { fromName =
                    "vK!_\DLE:\ESCI0\168602U\144178\b\NUL*\70679%\SUBvf7\59967\&7\1022395\51118\NULQn\1098780_\1052931]FIF\NUL\994410m?a\DC1\134034+\US\1016849[U\1056197v\rU$:\986190\SOm[\987847\1007064\DC1H\DEL\ENQ$_^e8e\1085721E')y\33670\EMR\v[Z\f)\SI\DC4\119067\137276\1039160c;'\170985\1064339\51122\RS\43522\ENQj\8110\1098421\\\133676PL|n\ETB\984318\1038283"
                },
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle = Just (fromJust (parseHandle "chuc8zlscl1gioct")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
          },
      newBotConv =
        botConvView
          (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
          Nothing
          [ OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
                        }
                    ),
                omConvRoleName =
                  fromJust (parseRoleName "zv9nb4emt5hh_59ezmb7gy7vex5csr4hizv2bzuj67mjuwx2wc4zf_8valch1hkjc")
              },
            OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))
                        }
                    ),
                omConvRoleName =
                  fromJust
                    ( parseRoleName
                        "pnj4jsurytr8p6wkxo1_1c8frkgjemx0y48aribcevovmbpeh2us5exkz_fkyfciz88zqw4z4f56orrphp2d5owojj7vxuus0db0eud_bci52125vmt"
                    )
              },
            OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))
                        }
                    ),
                omConvRoleName =
                  fromJust
                    ( parseRoleName
                        "3cwtdmxs2zcpv4k55pxg6354ab_2oqoz_jtetp3_u8rjfzac7jiq14oq24axxupapg08njxccrvix5b9q2r3ezmdsni5yx0oq55am8jeqv57815l5td3groa6vjm408"
                    )
              }
          ],
      newBotToken = "\US",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.SK, lCountry = Just (Country {fromCountry = ML})}
    }

testObject_NewBotRequest_provider_7 :: NewBotRequest
testObject_NewBotRequest_provider_7 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000")),
      newBotClient = ClientId 9,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
            botUserViewName = Name {fromName = "]\98090\DEL\SO\GSq{9\143048j\135048"},
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle = Just (fromJust (parseHandle "kfgs")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) (Just "\24918") [],
      newBotToken = "\DC4Y&;",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.LN, lCountry = Just (Country {fromCountry = GR})}
    }

testObject_NewBotRequest_provider_8 :: NewBotRequest
testObject_NewBotRequest_provider_8 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000003")),
      newBotClient = ClientId 3,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
            botUserViewName = Name {fromName = "0H\164007\1094020\CAN\1063257\v1\1064417\1068260(r"},
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle = Just (fromJust (parseHandle ".x1v4")),
            botUserViewTeam = Nothing
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) (Just "\DEL") [],
      newBotToken = "",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.TE, lCountry = Just (Country {fromCountry = AR})}
    }

testObject_NewBotRequest_provider_9 :: NewBotRequest
testObject_NewBotRequest_provider_9 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000003")),
      newBotClient = ClientId 2,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
            botUserViewName =
              Name
                { fromName =
                    "\70171C:`\127071\STXuO]\ETB\184168\118848\135471,)\1068222\fOy f\NAK\SOH!MhT\1080053zM\\W#\n\151257\SUBh\\\50212\1051875{'ok\190166\&0\145023\175772\EM)\1082496-\169085ZC\DEL\DC1\SO'\SOY\65205\RS\NAKm\\J:{\18670\FS,\1078706oNy$9DXX\ETX,#+\NAK\DC4\158270Q.+\CANC4"
                },
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle = Nothing,
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
          },
      newBotConv =
        botConvView
          (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
          Nothing
          [ OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))
                        }
                    ),
                omConvRoleName =
                  fromJust
                    ( parseRoleName
                        "18dmoaegl2lj3k9vvtivedw5umrfl3frcwsiv2f9wyhe66qgaeuzbxh_q5ja4sebpu9ofj826ufgeozzz5_0mt2kbnrl9fqxl9nfmgtbklecosycpw6fupemw7vj"
                    )
              },
            OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))
                        }
                    ),
                omConvRoleName =
                  fromJust
                    (parseRoleName "9vzqc64t8n6lfdea9ryucq_xu4x_v8mgjkv0jf8d5r34wxgac7yhqtnqnxivdzyhgotkpum07frl")
              }
          ],
      newBotToken = "\1020342X",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.HA, lCountry = Just (Country {fromCountry = MW})}
    }

testObject_NewBotRequest_provider_10 :: NewBotRequest
testObject_NewBotRequest_provider_10 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000004")),
      newBotClient = ClientId 0xc,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
            botUserViewName =
              Name
                { fromName =
                    "\28714+w\1052759*KHRC\DC3\DC2\69702\&0\1043100u1vT\ACK\94716\SUB}\65128\"P\1054449\&3\fb_\CAN\EOT\133649B55t\SUB\29069\&8\21614\1091434I\166155\135568\29529\1084846\SUBf\1077482\SUB\9091\151919\&3\GS?U\145649\SI0\1046380\996945\&1\ESC\STX8\46655g\146307\1068045?|\GSn\a+8|\166543#H|+\1054950|\1082601\1070384\&86o\95174"
                },
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle = Just (fromJust (parseHandle "hy4dc")),
            botUserViewTeam = Nothing
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))) (Just "\ENQ") [],
      newBotToken = "\18582h",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.RW, lCountry = Nothing}
    }

testObject_NewBotRequest_provider_11 :: NewBotRequest
testObject_NewBotRequest_provider_11 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000000")),
      newBotClient = ClientId 8,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
            botUserViewName =
              Name
                { fromName =
                    "\1034857\ENQ<\ETB\1067175`pv6$?U1\f\1061\900\&6GB\SUB\154475\1039582{W@\1013922\1106400w\1040667Z\trO\1058683e\66911\25986x*YUj\nf\53235lg\ESCs_\1046674S2[\DC2e\1101653\1004868=\CAN\36589,#\1035811\1105438\DC2{2>\DC3*\EM\23235%\bfn\180748\&9<\ETBc\181499\69937Qr\146682\n"
                },
            botUserViewColour = ColourId {fromColourId = -1},
            botUserViewHandle = Just (fromJust (parseHandle "pt-g.o")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) (Just "") [],
      newBotToken = "a",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.CV, lCountry = Nothing}
    }

testObject_NewBotRequest_provider_12 :: NewBotRequest
testObject_NewBotRequest_provider_12 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000003")),
      newBotClient = ClientId 0xc,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
            botUserViewName =
              Name
                { fromName = "F\1099815ar-'(K\30712\USOEED\DLE2(\ESC[\ETB\EOT2]&W\v\53091\995482\&8\1003203Hxl\184821\f"
                },
            botUserViewColour = ColourId {fromColourId = 0},
            botUserViewHandle =
              Just (fromJust (parseHandle "2mbu57j9i5av3tl5qq3defu9ydjatm7y-bgi4nznqyvcbmdn66pma5ice6famcazb892aqtzz2_zclckldrjh6nq69sz_2p0qx99p6t2ogt9ewzzq2olgge32jyt6kmwgmzvdbeti-iygnitchblkicol8m83a8n-a2ip-yy27z2llzu7")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) (Just "") [],
      newBotToken = "\49690\RS~\SOH'",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.SM, lCountry = Just (Country {fromCountry = MO})}
    }

testObject_NewBotRequest_provider_13 :: NewBotRequest
testObject_NewBotRequest_provider_13 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000004-0000-0003-0000-000400000001")),
      newBotClient = ClientId 0xe,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
            botUserViewName = Name {fromName = "6`k)?\189080V"},
            botUserViewColour = ColourId {fromColourId = 0},
            botUserViewHandle =
              Just (fromJust (parseHandle "7g_a0on27rzpz7cfzl3hle6v7dwv.db.to.ief5xzr3eu.vr5jb57_z5t3ahmggm9oddsd-quxc1uv4xkr7ncg9ff9zicgsjenafoxe4jbtrzjagqy84xrvt7iv_dcpe7_iiyg3tpeg8fh2osxf7dv01ueygahrdokoa-2ya37r6g0b0u3j416qnnk.404lffdz")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
          },
      newBotConv =
        botConvView
          (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
          (Just "")
          [ OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))
                        }
                    ),
                omConvRoleName =
                  fromJust
                    ( parseRoleName
                        "f5kideyd0z_wa8k_u0o3wcgbx1iea5yqmkrz3vv86ehs77akep4ttw6eznzo7tefijy5zqxnzq8u4mghhp3m2pg9kqtxnaxukzw1cn"
                    )
              }
          ],
      newBotToken = "",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = FI})}
    }

testObject_NewBotRequest_provider_14 :: NewBotRequest
testObject_NewBotRequest_provider_14 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000004")),
      newBotClient = ClientId 0xa,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
            botUserViewName =
              Name
                { fromName =
                    "\"\161008Z9\b<l\998840g\1035405\RS\\\v%>\57817\94488\34531yX\SYN\989653/\SUB\SUB/B\1089073B\EM?\n\119029zz\1063844\1079191T\SO]\1045646\1020565d\b[\183600\&3\35869\US\1074551\985034BVTBC8&\t\1085747\135733aRR\1071408e <(]\NAK"
                },
            botUserViewColour = ColourId {fromColourId = -1},
            botUserViewHandle = Just (fromJust (parseHandle "ho")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) (Just "\175323") [],
      newBotToken = "uC\SUBY",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.NR, lCountry = Just (Country {fromCountry = AT})}
    }

testObject_NewBotRequest_provider_15 :: NewBotRequest
testObject_NewBotRequest_provider_15 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000004")),
      newBotClient = ClientId 7,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
            botUserViewName =
              Name
                { fromName =
                    "s9FA\SUB+x\FSg\EOT\1007813i\47338\v x\RS\1054265\1022682\RS\1072491d5\65315q\SO!\SUB16\49941/)lY*{{\NUL\1113145\53420Y\DEL6i8l;\1025928\GS7\ENQi\CAN\1080655l\EOT\94393XH=\1089954\DC3\145036C\1092186w\404%Q2\US]r\1068741FL\123591*F\1008201l\RS\985036\SOf\fK\99663\DC3*6\48034{\1090532\DC3\DC1M\1074994oE\92342\ETBr^n*\SO/\DLE\1065124\DC3\f<u^s\\\992208[\1074697]\1088143\SI\38930\ETB\99073\DC1"
                },
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle = Nothing,
            botUserViewTeam = Nothing
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) (Just "\1046069") [],
      newBotToken = "",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.CO, lCountry = Just (Country {fromCountry = KH})}
    }

testObject_NewBotRequest_provider_16 :: NewBotRequest
testObject_NewBotRequest_provider_16 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000003-0000-0004-0000-000200000003")),
      newBotClient = ClientId 9,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
            botUserViewName =
              Name
                { fromName =
                    "N\ESCE`W:\"9\"\14840\DC2g_\"<\1047945\1062839GGQ/g\54646*\1005815|Sh)-\DC3&e-Y&&:\147317\1053744TWo\ETX\1010161\1009736@\SI>q\ETB\11622c\1068700|k\SOH\1090490 Dqwr\SI r\30804\161971\1014628?u\1021253AH\64817A\SOH\181530\1052127\SOHF\997870V\ACKkY\997171-\1081803\998604]'"
                },
            botUserViewColour = ColourId {fromColourId = 1},
            botUserViewHandle = Just (fromJust (parseHandle "o8opul3h")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) (Just "") [],
      newBotToken = "=\131697\163501e\83335",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.HI, lCountry = Just (Country {fromCountry = TD})}
    }

testObject_NewBotRequest_provider_17 :: NewBotRequest
testObject_NewBotRequest_provider_17 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000")),
      newBotClient = ClientId 1,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
            botUserViewName = Name {fromName = "j>\FSO\40436\1008903(.R\1098591\1057916O"},
            botUserViewColour = ColourId {fromColourId = 0},
            botUserViewHandle = Nothing,
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
          },
      newBotConv =
        botConvView
          (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
          (Just "")
          [ OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))
                        }
                    ),
                omConvRoleName = fromJust (parseRoleName "zi6nsx7hjs04d_1nxiaasqcb")
              },
            OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))
                        }
                    ),
                omConvRoleName =
                  fromJust (parseRoleName "c67nu5cxj9cru8018oquz_74mazgewq5fa6mwgwzktvep_7ftdtitzlwewqe")
              }
          ],
      newBotToken = "&))",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.NY, lCountry = Just (Country {fromCountry = NP})}
    }

testObject_NewBotRequest_provider_18 :: NewBotRequest
testObject_NewBotRequest_provider_18 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001")),
      newBotClient = ClientId 4,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
            botUserViewName =
              Name
                { fromName =
                    "\1038532\EMz\SUB%\139660__DO}\54713\50053\CAN\47274\DELZ\13914w8<\1009245\1001975\184118\ESC\32164{|\ACK3_)\DC3]f$\1112650;Pj0\ETB\a\DC2k\nG\SUBr\145903\&2}\DC3.\EOTB\SOH\CAN\162312\EOT\145691\ETB\1087729).\41256\tNwq\1022524\59021\1088435"
                },
            botUserViewColour = ColourId {fromColourId = 0},
            botUserViewHandle = Just (fromJust (parseHandle "gcmc3fjd3ire.maquq87awi")),
            botUserViewTeam = Nothing
          },
      newBotConv =
        botConvView
          (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
          (Just "\DC2")
          [ OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))
                        }
                    ),
                omConvRoleName = fromJust (parseRoleName "a8r6vcnbte4ouwljafu5fid9r_")
              },
            OtherMember
              { omQualifiedId =
                  Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) domain,
                omService =
                  Just
                    ( ServiceRef
                        { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
                          _serviceRefProvider =
                            Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
                        }
                    ),
                omConvRoleName =
                  fromJust
                    (parseRoleName "05bh82wu2bogl1wfzvdrt6l37s_1awtp4rbb5qyk9f2fezt8gq0u_f2eoa7qjloopp4yh0dg5h0ad")
              }
          ],
      newBotToken = "\175470\1078918Nr\1056432",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.GU, lCountry = Just (Country {fromCountry = SY})}
    }

testObject_NewBotRequest_provider_19 :: NewBotRequest
testObject_NewBotRequest_provider_19 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
      newBotClient = ClientId 6,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
            botUserViewName =
              Name
                { fromName =
                    "\172436\SUBM\NULz\1036939Wh>k\1013675{n\47782\f\23231;OG'\43870l8,\1065083\&5\1013071+P\n\6514itP\DC3={?\20208z\94534\54598z\7611z \1057751V;\1072041]o3\1027935id]\US\1086408\172854x\1004633=\1007221\1105556\DC2\DC32~;"
                },
            botUserViewColour = ColourId {fromColourId = -1},
            botUserViewHandle = Nothing,
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) (Just "w") [],
      newBotToken = "",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.ZU, lCountry = Just (Country {fromCountry = AO})}
    }

testObject_NewBotRequest_provider_20 :: NewBotRequest
testObject_NewBotRequest_provider_20 =
  NewBotRequest
    { newBotId = (BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000002")),
      newBotClient = ClientId 5,
      newBotOrigin =
        BotUserView
          { botUserViewId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
            botUserViewName =
              Name
                { fromName =
                    "Oo\f7\64177$\NAK\DC1^\DC3\DC42\58435\1005744\ETBwRy@!\154949\EM1r\FSw\a'Gd?\RS\1095298L{K\44115-?-\19125}l\158899\170957\ACK\190821Zk\163160XD"
                },
            botUserViewColour = ColourId {fromColourId = 0},
            botUserViewHandle =
              Just (fromJust (parseHandle "th4n3ndvnpp49es-gz55m5nnya_d.mcna7zg2t-t.xhcz6xbh17cg0.trdfgmo8whrtkl9fqdi8jg7d3nlh03p.bpumzn-.89h4.i75x6gx.x7kos0x4hqc.31hy78ckr6502kun7u7_b1a.8mw3oo3ylv.k29_zei793az7xlfaes1wa2gvu4tad52v5-w8rz9o-ivftxq5-nz87uhlm")),
            botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
          },
      newBotConv =
        botConvView (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) Nothing [],
      newBotToken = "\\`\ACK,<",
      newBotLocale = Locale {lLanguage = Language Data.LanguageCodes.NY, lCountry = Just (Country {fromCountry = KN})}
    }
