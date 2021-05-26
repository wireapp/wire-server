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
module Test.Wire.API.Golden.Generated.UserProfile_user where

import Data.Domain (Domain (Domain, _domainText))
import Data.Handle (Handle (Handle, fromHandle))
import Data.ISO3166_CountryCodes
  ( CountryCode
      ( AG,
        BB,
        CI,
        CL,
        ES,
        GA,
        GL,
        KE,
        MN,
        MO,
        MU,
        SH,
        TC,
        US,
        VG,
        WF
      ),
  )
import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import qualified Data.LanguageCodes
  ( ISO639_1
      ( DV,
        FJ,
        GN,
        JA,
        KR,
        KS,
        LU,
        MR,
        NY,
        OC,
        OJ,
        PT,
        RN,
        SV,
        TG,
        TO,
        VE
      ),
  )
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Qualified (Qualified (Qualified, qDomain, qUnqualified))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))
import Wire.API.User
  ( Asset (ImageAsset),
    AssetSize (AssetComplete, AssetPreview),
    ColourId (ColourId, fromColourId),
    Country (Country, fromCountry),
    Email (Email, emailDomain, emailLocal),
    Language (Language),
    Locale (Locale, lCountry, lLanguage),
    Name (Name, fromName),
    Pict (Pict, fromPict),
    UserProfile (..),
  )

testObject_UserProfile_user_1 :: UserProfile
testObject_UserProfile_user_1 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))),
            qDomain = Domain {_domainText = "v.ay64d"}
          },
      profileName = Name {fromName = "\50534\3354]$\169938\183604UV`\nF\f\23427ys'd\bXy\ENQ:\ESC\139288\RSD[<\132982E"},
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = 2},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
              }
          ),
      profileHandle =
        Just
          ( Handle
              { fromHandle =
                  "emsonpvo3-x_4ys4qjtjtkfgx.mag6pi2ldq.77m5vnsn_tte41r-0vwgklpeejr1t4se0bknu4tsuqs-njzh34-ba_mj8lm5x6aro4o.2wsqe0ldx"
              }
          ),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.VE, lCountry = Just (Country {fromCountry = KE})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T06:42:08.436Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),
      profileEmail = Just (Email {emailLocal = "4", emailDomain = ""}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_2 :: UserProfile
testObject_UserProfile_user_2 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),
            qDomain = Domain {_domainText = "go.7.w-3r8iy2.a"}
          },
      profileName = Name {fromName = "si4v\999679\ESC^'\12447k\21889\NAK?\1082547\NULBw;\b3*R/\164149lrI"},
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = -1},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
              }
          ),
      profileHandle = Nothing,
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.NY, lCountry = Just (Country {fromCountry = MU})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T01:42:22.437Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
      profileEmail = Just (Email {emailLocal = "\172353 ", emailDomain = ""}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_3 :: UserProfile
testObject_UserProfile_user_3 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
            qDomain = Domain {_domainText = "jn0602-8rda.0s.484f421.ee7"}
          },
      profileName =
        Name
          { fromName =
              "\135845-\1101575B`Fdev9lK'=/\12962J~ZI\1020565^\ETB#\DC3x\158522Ng\USC\DEL4\1061388d+\b\RS*\DLEAj\SO\182099\6489\&6\1062187<\137130\22024#\1078084\1024687\7834~\53178\&4\1064101"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = -1},
      profileDeleted = False,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = "9w41opcty3"}),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.FJ, lCountry = Just (Country {fromCountry = US})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T21:15:22.178Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))),
      profileEmail = Just (Email {emailLocal = "\EM\ESC", emailDomain = ""}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_4 :: UserProfile
testObject_UserProfile_user_4 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
            qDomain = Domain {_domainText = "6s4blca8bh.6u.59p4f.j18151"}
          },
      profileName =
        Name
          { fromName =
              "~\1107615n\1092584/Xo0/\127257Y\1866!\46553|\GSm3\180366(\1047928g\a\15091f|t\61950>3\168146\&60\20575\1070466\159251-\141952\&8\r\181611<\1102854Gvz[\ETB\1112766x\v\1107261H6\1068126\SOH\f0-0dL\1026774E{5\5921i\1039789\988467\&1\\\136616\SUB\SO\afW\33397S\1104118\DC4\ETX\au\187084\STXE\ACK\69915\190154\51438\8403\9315\FSv\f\1061477\&8E\tb5\42467\&8)\SOS\USiy"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = -1},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
              }
          ),
      profileHandle =
        Just
          ( Handle
              { fromHandle =
                  "dauxfkc4f7s4ut0xhxnq9l8zzpeuze998esch51.vh.t56sr1j8bavtco.40te65.sl3b9yzgwxdpxld4_mnoou.adu0lcwxf63lmt8ev8ug7dy39ft31vweb7684k"
              }
          ),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.OC, lCountry = Just (Country {fromCountry = TC})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T13:58:16.443Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))),
      profileEmail = Just (Email {emailLocal = "\40237", emailDomain = ""}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_5 :: UserProfile
testObject_UserProfile_user_5 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),
            qDomain = Domain {_domainText = "awdt-0be-r.7hyxl8mkb.s.lp"}
          },
      profileName =
        Name
          { fromName =
              "q0\USe\147265\DC4e78h\DC1!E#}A\GS\177692b,\51312\1075556\179877\&2\SYNP\1075345\166498\1009760,(\DC3O0\94026\31436\nGXJ"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = -2},
      profileDeleted = False,
      profileService = Nothing,
      profileHandle =
        Just
          ( Handle
              { fromHandle = "2k1t1hdfpvdkxij-0w735w5xniggherg.c8d_be21d3mrasu9bkz38dmbwhca3neuduc0oz8v3n1-bd81z6ocf5d1i"
              }
          ),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.PT, lCountry = Just (Country {fromCountry = ES})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T03:37:18.107Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
      profileEmail = Just (Email {emailLocal = "/\v", emailDomain = "\176812"}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_6 :: UserProfile
testObject_UserProfile_user_6 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))),
            qDomain = Domain {_domainText = "87k.iag53"}
          },
      profileName =
        Name
          { fromName =
              "L\"v\FSk 2d$\USQm|\135151r\1049028\1056538\138866\1023394-\1008034\DC4+dC/n.\r\USAX\SUBs$B\DC1\32458G\33784pTC\159559Lgs\58928\EM\31200,vz\1022740\23748tYvC\DC3rA0{\100659e!\189204\135095'r&X9k4Z\98253\DC2YW*\177006Wh#\172479_\1080740\1055026r\NAK(\1094630\138346\1090119\FShF\ESCn^\186871=!\STX\1007566]`\1063058\1105955\FS\ETB=9D.x$\ESCU/\STXFJ"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Just AssetPreview))],
      profileAccentId = ColourId {fromColourId = 0},
      profileDeleted = False,
      profileService = Nothing,
      profileHandle =
        Just (Handle {fromHandle = "frz05jdtt5.dacr3hd-hmayzm93q91g-qbd-4rm2hs2de4jx.80xj9y6k.c70.s_s_85ymq0w.lv_"}),
      profileLocale = Nothing,
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T16:24:39.844Z")),
      profileTeam = Nothing,
      profileEmail = Just (Email {emailLocal = "l", emailDomain = "\74983"}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_7 :: UserProfile
testObject_UserProfile_user_7 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))),
            qDomain = Domain {_domainText = "uy-m.eb-p-ehu.n.t5p-i84u"}
          },
      profileName =
        Name
          { fromName =
              "0\\x\41194\1025998\RS\DLE\41260\177322\988734n\1025166\120094(w\31071J\157859\CAN~W|#!\1101827\110864\1054048\GS\1051711\&7\SUB\1106402/\1100826\147223\&6\1007341\190764]\SO\ENQIRy\186776/H\138633)\166423\1018759jEO\DC1d`]A\GS\"\US\136784\1107857Irh;M\1033302\20834LzC\7977N_\DC2\998835-q1z0\23277\137310\42710);\174333z\SUB5\1099606ZN\1112582\SO5\SYN \54701"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = 1},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
              }
          ),
      profileHandle =
        Just
          ( Handle
              { fromHandle =
                  "67-9yy9qo2p25zhd58xripmrgouiuww98mk.m5xfnlvqmpz8wsgjuo7eo149d_0is6w26-2zo4z1kbf6zmkth4n3j139iok0s80ccdfxcb-jy3edziep9hpb2r.glfme2q09t..ehpdjc57dv-9_8nt0f"
              }
          ),
      profileLocale = Nothing,
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T08:32:43.292Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
      profileEmail = Just (Email {emailLocal = "", emailDomain = ""}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_8 :: UserProfile
testObject_UserProfile_user_8 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))),
            qDomain = Domain {_domainText = "755m8o7.5-6tg.x48tvxc"}
          },
      profileName =
        Name
          { fromName =
              ". \18083\1031749\992124\\\1071115\STX'\SIV\1084248\1091205)\983738\NUL\1059015[EO\180607M\DC2A\175335\1014385O&\r\1086193\171482"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = 1},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = "dbiy"}),
      profileLocale = Nothing,
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T15:50:41.047Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))),
      profileEmail = Just (Email {emailLocal = "\984237", emailDomain = "\DC3F"}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_9 :: UserProfile
testObject_UserProfile_user_9 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
            qDomain = Domain {_domainText = "p.6u0.jym"}
          },
      profileName = Name {fromName = "Xa\SOHtP\"\161415)\23205\DC2\34102y\\]E\v\\G"},
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = -1},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = "72nnsdja3n"}),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.SV, lCountry = Just (Country {fromCountry = WF})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T20:54:55.730Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))),
      profileEmail = Just (Email {emailLocal = "_", emailDomain = "U\STX"}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_10 :: UserProfile
testObject_UserProfile_user_10 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))),
            qDomain = Domain {_domainText = "yg0u9.w-7-l.x6"}
          },
      profileName =
        Name
          { fromName =
              "E8VP6\\\STX\ETX\1084758\136489\17676\1102237\SUB\DEL\1056983+D`\991577\995184GEl<\GSU8y\1064857\NULguH%XBf}\\\DC4\1095683\163418\STXH(\FS\1111450\140458)$.*(p~n\157707j\ENQn\DC2\ESC]\65223\161758=)\180231\998932Y\ACK\1002339\111345\SYNE\32763\t\110997\8084vLXFigV`Z\SOHwa\133943}=\vS\1004479\40602\1035955EA&r\EMo6\1101427\60026\DLE\188008\GS\1002964v\1038845\128451\996282"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "\US" (Just AssetComplete))],
      profileAccentId = ColourId {fromColourId = 2},
      profileDeleted = False,
      profileService = Nothing,
      profileHandle = Just (Handle {fromHandle = "cxh8m"}),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.MR, lCountry = Just (Country {fromCountry = CL})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T12:03:12.917Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
      profileEmail = Nothing,
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_11 :: UserProfile
testObject_UserProfile_user_11 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))),
            qDomain = Domain {_domainText = "9p4.6cpl.p92"}
          },
      profileName =
        Name
          { fromName =
              "\151605\39087Z\n2\DC4y-\v\DELcS\ESC\SI^S:K\v{,}\NUL\161121i\162096\ETB\156507t\24702p};mY\DC1g&\1018745\1081629\USW\1056481\17501\1039160$\SUB6\1018210\1100674\tz\92652+\DC2J\1017525\1090958\1058616\45670\1000384c\45329B}\176309E\48361\ETX<D\49685\176804\t_i\67167a(M#GgJ\SI\179757q2CB\ENQ\189296\USA*\1033651\1036514\n=\b"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "" (Just AssetPreview))],
      profileAccentId = ColourId {fromColourId = -2},
      profileDeleted = False,
      profileService = Nothing,
      profileHandle =
        Just
          ( Handle
              { fromHandle =
                  "nxd4m3jnqbghht70om5xn4bbnb_vres47msry0v.1so2.61d4_c01xrazl.bo3wdqz..z5ydp73gle3hshmd0u5ji8802q761ce87k_gkinka8q1vxkm9rht3i-k8qh49s46sg3hhmmhy6ucx0jb0xf9dci93r44g8y0m8mmyucamtw7et.eh8uca-2lig2ayy-747_i69-1m1txhz8dw-i02w0bhyz8sjbqlqu25e6n_2nmw00kp_ob"
              }
          ),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Just (Country {fromCountry = GL})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T13:14:40.847Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))),
      profileEmail = Just (Email {emailLocal = "\995640\EM", emailDomain = "+"}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_12 :: UserProfile
testObject_UserProfile_user_12 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))),
            qDomain = Domain {_domainText = "7.jw8.la-0.r"}
          },
      profileName = Name {fromName = "\"EBhv#=\135744qrx\STX>XR\1063545B\USW\21733d\SO-\SO\a"},
      profilePict = Pict {fromPict = []},
      profileAssets = [],
      profileAccentId = ColourId {fromColourId = 0},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = "ml"}),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.KS, lCountry = Just (Country {fromCountry = AG})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T20:48:09.740Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))),
      profileEmail = Nothing,
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_13 :: UserProfile
testObject_UserProfile_user_13 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
            qDomain = Domain {_domainText = "1.z6lionkv7"}
          },
      profileName =
        Name
          { fromName =
              "u\DC3\34311T\t\US;\989746[\"n`\"\NULD\r\163171 Vj\SOj>Pb\1050733]\143998\995637I\n\CAN\1078162\&6\SOHx\"p\38430E\CAN@w\1101399\100497R\DEL\vX\157034\176201\1105646@K|K\1039270\&4\51976\166287\STXo9"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Just AssetPreview))],
      profileAccentId = ColourId {fromColourId = 1},
      profileDeleted = True,
      profileService = Nothing,
      profileHandle = Nothing,
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.LU, lCountry = Just (Country {fromCountry = VG})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T18:04:28.203Z")),
      profileTeam = Nothing,
      profileEmail = Nothing,
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_14 :: UserProfile
testObject_UserProfile_user_14 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))),
            qDomain = Domain {_domainText = "9f97og.h2af889e64zw"}
          },
      profileName =
        Name
          { fromName =
              "\182072d\1064502xu\41418\EM\STX\a\ETB\DC3.\DEL\t.7k^!5*\1002638iT)\EM\1076820\1031217\55164\r\GS\78808~j\US\DC1X\SOH\DLEf\1070389Vo\FS7]\ESC\171525J\172010\1091730,-K\178947w`\13664\25008\EOT^>A\n\37142C\120407v\1065338R1Q\998858\CANU\ENQP\t\FS8~\CAN\185286\EOT\152846\182973\185538\a_Kdj\50578l\DELN+L?\1005665-\US\1055296wGP\59781.Zm"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))],
      profileAccentId = ColourId {fromColourId = 2},
      profileDeleted = False,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = ".0f-ea"}),
      profileLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.OJ, lCountry = Nothing}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T01:21:51.302Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))),
      profileEmail = Nothing,
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_15 :: UserProfile
testObject_UserProfile_user_15 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))),
            qDomain = Domain {_domainText = "q64o7.302d44.yvc0-7.61.v9mv"}
          },
      profileName =
        Name
          { fromName =
              "\1021630-\1011751\10544\NAKS\1069392\SUBj\135463tu\\\1009418\180291\t\RS\163835\164989\1008429\FS\ETB\ad\ETBeT\DEL\SO$\1073140YT_e\188102!)\SI\DC2S]\1107330SYm\52680g\NAKpX,GQ\DLEF\SIJ\61777\SO\b\vR\GS\STX\25359\1082212y\CANg\36697J\NAK\"\fMX\994950\&3g\1093512n4\1052736\US\121065\DLE"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))],
      profileAccentId = ColourId {fromColourId = 1},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
              }
          ),
      profileHandle =
        Just
          ( Handle
              { fromHandle =
                  "3ti0segu2s7u294jpsv5lixp_axfvix4h.a9phbpuprb5g1.g5a-5hh7ezf2ur8z.2pms57_cv2pupxv8xdgnvbqyx_eiyk3k0bsky53x1fzivty4j.c79jfo9zr9f8pru1j-ixl67d9cz23b1e-m603_iz8_2uf8neudzfq1vi1ec"
              }
          ),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.DV, lCountry = Just (Country {fromCountry = MO})}),
      profileExpire = Nothing,
      profileTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))),
      profileEmail = Just (Email {emailLocal = "F", emailDomain = "\NUL"}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_16 :: UserProfile
testObject_UserProfile_user_16 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))),
            qDomain = Domain {_domainText = "19.i.mwv-7"}
          },
      profileName =
        Name
          { fromName =
              "\19513\48782%P\10154\a\1093017\1051048?.\337\52663\SOH%\5578\140305\1010727\140336\42232\1018530\10555`)\1090314(&\1040005W\27963\165377\1086495Sh\NUL\100787\1013554\NAK\EM\NUL\1016363\1029788\EM*vUWp=/H:\SIP\144251K/Q}\1056651(\994178Z\161956)_\1004918O\1033630\CAN0\1047955\999216g>\1025653\SO\SIW\1002557h;\78320\SIyJt\SYN\70205(q}\40502:b\132308/\RSGV\1053156\DEL{r\14107$oo1H\1028116n\b\42587\1104360hBiS\1065487\&6Az\DLE'\1053065="
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Nothing))],
      profileAccentId = ColourId {fromColourId = -1},
      profileDeleted = False,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
              }
          ),
      profileHandle =
        Just
          ( Handle
              { fromHandle =
                  "guo2m_9jgnm.snxp7uazht2j6je41-be6p5y9g-0afnnll.k5x0al45l948yuys97uy.7azgirru0r55.h-2ylot.j.y433-1jdw_ecazkzeqpa4lr.zt9.zjjaz7bzvxo4baqtod48_388s6-vxifhn9giwz6nc290fw2589psan"
              }
          ),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.JA, lCountry = Just (Country {fromCountry = BB})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T06:39:38.220Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))),
      profileEmail = Just (Email {emailLocal = "V", emailDomain = "|"}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_17 :: UserProfile
testObject_UserProfile_user_17 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))),
            qDomain = Domain {_domainText = "94nvo.97j.g83--2e.4-t1g.z"}
          },
      profileName =
        Name
          { fromName =
              "@@v\139742}$2^]\983599=\a6\164216\DLE*\f(\CAN(76kCs\ACKX[~\63041\30173\NUL\NUL\1100513\1094624o\51999a\US0E|\"d\152042\SUB{!X\STX\aF`\ETB\EOT*\985434\73863;#tOw\SOHD\1087790 \1068210E\n\CAN\DC3D`>\DC3\ETB965!CL\1107341\1011690B9\US\DLE[\1041774\&0\1093885%\f.y\RS\23872\a\ETB\33686\RS[\32348\1010958\ACKPuC"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "N" (Just AssetPreview))],
      profileAccentId = ColourId {fromColourId = -2},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = "uva.d"}),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.GN, lCountry = Just (Country {fromCountry = GA})}),
      profileExpire = Nothing,
      profileTeam = Nothing,
      profileEmail = Just (Email {emailLocal = "I\t", emailDomain = "%"}),
      profileLegalholdStatus = UserLegalHoldNoConsent
    }

testObject_UserProfile_user_18 :: UserProfile
testObject_UserProfile_user_18 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
            qDomain = Domain {_domainText = "r71-2.u"}
          },
      profileName =
        Name
          { fromName =
              "$\1006024\ETBq\172753\NAK\1003120!\1078351\DC4\990161\EOT+X\190655\997166n\FS\DEL\SUBr0\1087799\1061617z5\191342\&6\992189mBxr\a-H\173621\8826\SYNf\163769\1008130\1069053EMAQ\189593hm\"t\1005327\169220=\NULh$:>\1042739E\26278\120919lz\185436oY\te\DC17B\1077329Be\n\f\1057987/\ETBo\SUBw\1089339\35004\988396\1061645R\161579\EM\DEL\189459\54402\ENQ\39234v\ETB\1090963\63379\180155\US\993694dj=\nZM"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))],
      profileAccentId = ColourId {fromColourId = 1},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = "50-a8ceq8yfwac.lp."}),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.KR, lCountry = Just (Country {fromCountry = MN})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T13:37:48.974Z")),
      profileTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))),
      profileEmail = Nothing,
      profileLegalholdStatus = UserLegalHoldEnabled
    }

testObject_UserProfile_user_19 :: UserProfile
testObject_UserProfile_user_19 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
            qDomain = Domain {_domainText = "00.252-t7.7l7.r478qz0a4.b"}
          },
      profileName =
        Name
          { fromName =
              "\v\1041473\ACKW\vKmM3,j\1003909uX\184886\GS\63083\a\SI:\1040232\151396m\ETB@c\1055742ePd=Bhs:\NAK\NAKa(\1051834hyWvEO\1023717Q\US\n\190342\1061646y)\1033452Y\1073361\1601}\\\SI\DLE\RS\177923mP!z\1000637c\aqMFa\ESC\1068568"
          },
      profilePict = Pict {fromPict = []},
      profileAssets = [(ImageAsset "" (Just AssetComplete))],
      profileAccentId = ColourId {fromColourId = -2},
      profileDeleted = True,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = "2g"}),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.TO, lCountry = Just (Country {fromCountry = SH})}),
      profileExpire = Nothing,
      profileTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))),
      profileEmail = Just (Email {emailLocal = "a", emailDomain = "\b"}),
      profileLegalholdStatus = UserLegalHoldPending
    }

testObject_UserProfile_user_20 :: UserProfile
testObject_UserProfile_user_20 =
  UserProfile
    { profileQualifiedId =
        Qualified
          { qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))),
            qDomain = Domain {_domainText = "2.ei5-x3"}
          },
      profileName =
        Name
          { fromName =
              "fB\DC1\1107358\66001Wg_!\ETB\DC4oZ\180871t<\FS\tI\1073000\2742r`\SOHO\131716+n\121207\4754]`dR\SUB\DC1z2\1084540-.4H~\36039|\98420<\99835\987986^~\1003487gR,\f\NUL x)`D@,\ETBv\160408\1105176\&1%@#R}(C2OCy\t2\7837\SUB\GSgs&d\984681\172116\SOH\1013326^"
          },
      profilePict = Pict {fromPict = []},
      profileAssets =
        [ (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Nothing)),
          (ImageAsset "" (Nothing)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Nothing)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "" (Nothing)),
          (ImageAsset "" (Just AssetComplete))
        ],
      profileAccentId = ColourId {fromColourId = -1},
      profileDeleted = False,
      profileService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
              }
          ),
      profileHandle = Just (Handle {fromHandle = "zticd1l"}),
      profileLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.RN, lCountry = Just (Country {fromCountry = CI})}),
      profileExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T10:18:53.031Z")),
      profileTeam = Nothing,
      profileEmail = Just (Email {emailLocal = "\1000346!", emailDomain = "|D"}),
      profileLegalholdStatus = UserLegalHoldDisabled
    }
