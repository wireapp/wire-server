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

module Test.Wire.API.Golden.Generated.NewService_provider where

import Data.Coerce (coerce)
import Data.Id (Id (Id))
import Data.Misc (HttpsUrl (HttpsUrl))
import Data.PEM (PEM (PEM, pemContent, pemHeader, pemName))
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import Data.UUID qualified as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Maybe (Just, Nothing), fromJust, fromRight, undefined)
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
import Wire.API.Asset
import Wire.API.Provider
  ( ServiceTag
      ( AudioTag,
        BooksTag,
        EducationTag,
        EntertainmentTag,
        FinanceTag,
        FoodDrinkTag,
        GamesTag,
        GraphicsTag,
        IntegrationTag,
        LifestyleTag,
        MediaTag,
        MedicalTag,
        MoviesTag,
        NewsTag,
        PhotographyTag,
        PollTag,
        ProductivityTag,
        QuizTag,
        RatingTag,
        ShoppingTag,
        SocialTag,
        SportsTag,
        TutorialTag,
        VideoTag,
        WeatherTag
      ),
    ServiceToken (ServiceToken),
  )
import Wire.API.Provider.Service (NewService (..), ServiceKeyPEM (ServiceKeyPEM, unServiceKeyPEM))
import Wire.API.User.Profile (Asset (ImageAsset), AssetSize (AssetComplete, AssetPreview), Name (Name, fromName))

testObject_NewService_provider_1 :: NewService
testObject_NewService_provider_1 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "\64037!F\163618\SO\bdU=JA\EOTjQah\r\SUBb|W\988921\DEL\NULw\1097305\181890*4\1044577\&6\132632/\1096220\CANH\47957LW\1070047\19540sG\NAKl\26599a\ESC\NAK;okw\akaN\146862\83301DAG8pVg\US;\95865@O>Y\1048383M\142783a\78862\992589\DEL[C;iS%brix\SOH\\h\tp\177234\1014187^\40547\DC4\172171$"
          },
      newServiceSummary =
        unsafeRange
          "~\1005150)\FSF{!r\t6k\1099103\"f\18575S0`\139733;\GS`B\17974\96421\ETX\136860I\DLE}\STX\CAN\EM,)\1024266>a\182749B\1008070C\188588\1066736\1020866\RS\187262#\12395\1003453mk\173053\b.\t5i@<\29462\120330\"CQB\127758",
      newServiceDescr =
        unsafeRange
          "\1090450dd\153790\1098765i\ETX\20031\1067227F\DC13$6I\7151{\2310\159580\990061e\bm=xO*\983561\1090062t1\DC3QZ/m\128270\SOH\DC3)w\ENQ\ACK\ACK\f\1023626\179346<\99491!\46340\1008748r\1055812{e\1049770;\985158MV\DC2^Va%\1102913Mx~w_B\ETX\t\1102876\NULVf[eE8\NULOU-\EOTgY\SUBV\45306\188060\NUL\ENQO\ETB\SI!%\RS#\175058?\1094156w\b,T>X_3S7\1055292O\183739\1071316c`\NUL\1002347L\SOXv\CAN\99046w;`'C\t\US\1006088\SO\ETBA:M\USHh\63667\1068356&zF'\DC4\SO#\"ErZ\EM9t\DLE\EOTh9D\27030\50053\DC1",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "uw=="))),
      newServiceAssets = [],
      newServiceTags = unsafeRange (fromList [MedicalTag])
    }

testObject_NewService_provider_2 :: NewService
testObject_NewService_provider_2 =
  NewService
    { newServiceName = Name {fromName = "CmN1\ACK"},
      newServiceSummary =
        unsafeRange
          "\ETBDTO\"\9060)\vo\36781\17609\v\1008456\169567\SI \183577\&7\US\36589\NAK\ETB\1032953q\RS\1003616\187836",
      newServiceDescr =
        unsafeRange
          "I\1113017\ESC*6\24177f)B#.cDw\1109197w\FS\EM#3\131727\1085999\SOHLW/a\13510AXRm*ud*0\177062\DLE`gu\153227\175142-\fTW>\DEL(\SOB>\DC3L$.\EOT\DEL\ACKIR\ENQGV%\1031815/~\1053512\8337\SOH6y;##\EOT\61009\&2\ETX\189764\&4l6l\36498\1090277\1038625\aNi\51564\DC2mS\1002459L\1053521~\1025213L\SI\USvT)\175422\ENQn\1089803r\1102927\ESC\1002298\&7\CAN1-\EM\n<\r\18399\992161\FS \1076069\92664/\35105\1015984\ETXabW+\FS$\24774.~/\US5r\ENQ33\\\GS%i#\54531\135581\12829\&0\b\1057417B\SOH\DC2~\42653\1044594\\q\62486qXub\40185 +$\1083113\132294MxpdY\STX\"\42305u\178236\&7j&Fpt\b\SYN\143516\99096C?YO13\149976^\1102356\CAN `h\vL\57614\1102085TY\1050764r\138807\1042814^]\NUL( <7WI]e^ix~\1097308\CAN\ETB\35072:\166983/\1047403=\RSt\1019543\66512,fq[r;{+\b\166774AxH\1029471T\\\100781|x>!\61446\t\FS\ACK7%\140210Lj5 \46316.Z\1073577x\EOT^M\1070141[\135341\\N\SOH+eW\FSu\16051\CAN\1054258\163662&\182151E5\150174\17474tPX9@VnH\1025548<\59344\1105793_\DC4\40966{\22693Cg\ar\STX\990337<Y;\SOH=NM!\1042188\&39\ACKu\DC1\DC2\46054\a$\SOz\ETB+xZ=.$\STX\RS\1087785\917940\1041899j\813\b8y\95949f<\8234e\RS\71351\1096428xR)\ESC\1094439\&4p\25020\SOj\1068604\1049591I\134662\n\v\STX\166943p\62433\1061863Ukpp;",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "Dag="))),
      newServiceAssets = [],
      newServiceTags = unsafeRange (fromList [FinanceTag, GraphicsTag, NewsTag])
    }

testObject_NewService_provider_3 :: NewService
testObject_NewService_provider_3 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "ER\1581\SOH3gq\DC3L\1042714\ETX\1093808+\1100909;Bv\n\STXb%\170481\&7;\SOH\USMJ'\986288k>\1017144V-=zt2H\180783\EM\ETB\1016783!\1029184o4\1092706\ESC\STX\CANE\DLE\tE e\1093890\ACK\141830\&7xM\1105693D~A^\US\1056861X,BFIJ\NAK>\\@M_j4\DLE\156652\b8S\153703NPC\991083\"M\DC2pc\\c7\1019941k!q\r/\1097064yy\1093106X@EF\DLEZ\1075941%W\37187\984787"
          },
      newServiceSummary = unsafeRange "C5\DC1\1108643\t|2\1011885\999412\152763\1081449,)\EOTUQ\1091976\&2u",
      newServiceDescr =
        unsafeRange
          "h\DC2U%\1054413sb}\17685\1053310\1074213\73041\10024\1085198\FS\1041498\DLE\1074566\&3/\ENQ\SI2Xx\FS\48146\US\45496x\12106`, Zz|HSQjO\1050611\181269!{\1068329\1003666\rF[W&\161597\1082928\1059527\n\22892\1114020$'\190311%\1102384HLQw)\EOT@R\141947e#\1039992b)\1014708\&7\1090255\44842\GS\ESC\27105\&1\SYNn\DLEn\\\133358S\1077749ks\1059593,\149685\19373,O\NAK\1049758\DC3y,\133010[zI8[>FY^&\1002570i!\17988%a\1090572\188827/H\"9\1045799mn\rSi +.UR\133273\1083461F3U\f\31021\39179\SO\184201L\156510c\STXEAW\23574\119213eWNmn\161111o\182150\ESC\GS/\179544\b\1050429@\"b\EOT\SO?\DC4Q\DC3L5\1100319!\996914K`\FSS3R\ESC\DC4",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate ""))),
      newServiceAssets = [],
      newServiceTags = unsafeRange (fromList [GamesTag, LifestyleTag, VideoTag])
    }

testObject_NewService_provider_4 :: NewService
testObject_NewService_provider_4 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "]~~\SUB\1089774\t_\25605\DEL\DEL<6\998975?lwcI\v\nw'\12193Y%yF=<,3\DC1&\ETXpw6O\SUB\184069_i\NAKt-\1802Jh%\CAN_\149867fy\993828\165704\NAKZ\1027899\38836f\1046496e-\175153\191238Dt>}0\NULryA\by\DC4H\SOH"
          },
      newServiceSummary =
        unsafeRange
          "M\4024@~\1023901\989634;!h\t\1051769W\ENQ\1063169W\1017992j{C,]\19092\DC3Tmm+]4\34936\151236\&5J\9470\191319\1054399\&4\994608\ACK\ETX!N\1003012\DC4\SOHp\r\28510\GS{3",
      newServiceDescr =
        unsafeRange
          "=Y\39298\&5@\SIy\1022879\nQm\1093830s\6934%\NAK{\997598|^)\r7{\1050865l\FSM?46\1347D)\1075391_\DLE9\f\EOT\vkl\NUL<\USirM\1077188\989558z\DC1\16765#\132403@|\15312\EOT2Y\991414BOsU\54169\1067621\ACK+y\167693\ACKJRaY\t.\1102513a[.\13696\NAK\137529\DC4Uw1.\SIc(\101095\DC4\ACK\b.\"\DEL,U\169122\ETB5\1101014=h4cAD\SOHh\SOOxB\f\78351f(\aMC\NUL@\1001705\&8&\US\1024587\t\160671\NUL\1107375\&1\NULE%\DC3M5i\30152\48207.\987943/\DC3\DC1+J\a\GS\DLEg+\1048977x\DC1\ENQ\168762\ESCu>`p\189121s\SI\1012058np\RSOZ2\168364:\58162x\17382\161845\1069584\aS\92633\ETXJ\1046593\ENQQ}\989246\166165\175333\SUB(\1100332\42409#h\20897Ut\1052\t2,&\1017284\1027308*\155188a=&(\1014134\\5\188713\152155\173237,y,",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate ""))),
      newServiceAssets =
        [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)],
      newServiceTags = unsafeRange (fromList [MoviesTag])
    }

testObject_NewService_provider_5 :: NewService
testObject_NewService_provider_5 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "\SUBy\1113668\16126s:\SUB\1061983\EOT\ACK8h\RS\1108776\989759V5D\NAK\180498\ETX\1020758\DC30\187040i\1110153b\137447b\NUL\NAK\GS\1017414.&nf\FS&],1+ed\1068415q\1034951&\bwl?h3?\66290\1057213 \52677'\1035465\170237|\1103483\35539\1030163\v\1094024\\\25839ehe>v\1080702K&\1075875lu7aa\SUB8BUg\EM\94818"
          },
      newServiceSummary =
        unsafeRange
          ";|H\EOTR\SOv/\a\1038730W\164744Zrdc\72117nJn\STX\GS\vcx%\ETB\FS!\1070663\1096795\NAK\EM\1095671d\26234+\ad\1039465I\n\CANQ(\ETXRN\DC2\133870\50202@)M!t\66010\38437\144984\ne:.\EOThEI\174694\97374\DC2.\DEL[6\1010284L\30483o\1095971\DC2o.\149411o\CAN:Q\"\DC4=Fi",
      newServiceDescr =
        unsafeRange
          "\b!\194863\1096342\1014993+\ETXQ\ENQ\28855\ENQy\1067519\&9F\54556}]a\1023081Sl0E\1098780Y~\1060366\95067\"@U+\vn\983384j\6098\&9\1024327ZZFA\1030002lTf\n(\STXy\GS'\49603/\152567^D_Qcq\998070A\1031272S%\CAN\EOT-\1111075}]me\ESCKc\119596\1065113ZE1\b=\186681C\aoIqvd\NUL&\ESC\986362\44324d\1095775TxJM|H\1095689\bnkA~o)\78031]\ETBQ\45880\48676\DEL\EOT\36963gZ\1051206CX\b~\1065236kw)\ACKQ(e\38971*\59998*\DC2\STX\1008586\NULf\DC2\1101929P\36860O\1068888*D>\RS\r_\\\SI\DC3\1103372SR\r\fM\1024207;!\1069538\1110830\&92\1057466\4319\1077125\&3U\986131S8\162182uJ.Z=\EOTW2\9535c\50725\132155r\rq;\DC4\74315\SIY\v\1016489uL\\\51270\1011664\SUB\1081295\1005625\ETB\GS\47630\1024933\&3y<OM\1102771/[\1038892\28225O\DC2L~\1073982i\bi\98390\1047545_\GSR\NUL8h\997905Nw\988403\995829\45881\1104293w:\SOH+\RS\t\1020523\&4\DLEL\18989\&5=C\DLE\8231~\DEL~R\18769\1100261jG\"\v%:bU\1031520\1104909\DC2\983357\163020,\1070403\171311,:\1041362\ETB\990327:-\194799\DC3\992960\ESC\989610[\ETX\n\1036378\&0\1013925\1046564\ETB\STX!X4YH\SOH\998953h5i\68676sB\DC1\1007495\158943\160951\175130\52942\nVSj\27748\1041296PV\139566\44806\1034074wyO~\SYNcT\RS\GS@\SO\r\1036391?g\1094345xs\991051\111106\144140\DC2\1086240d\13803\132882#\1102542\29909\a\1082789NT\CAN\1005322mk\167607\2631\DC2[lfx|-\31372Z\vux\135365\178826m\1080644\&1\36714\\GK\NAK!.\26084\&2)\DLE\SIK\1073023e\GS\45608\159408\120376\18077v2\97650\1077354pI</_\992041\DEL\nW\1025472\f\NULC\59142\1053538\&0b\ESC\b\169799\DC2.c5:@I3\135313\1025203c\21012\SUB\1094194\EOTd\1030094\20235b.\a\DEL/%\a]!\175191\7898|im71H\187511b\CANH\DLE\1051743\993689\vRy\v:\SUBAPiwgtq\1083008\52115P\td-~s\1052804\59769f\1039009\35451\128373\185329\b+\129576>OvPRFh\1080907\51559}\t.\RS\EOT6\1051302+X\135249\SO\14703\75069\1013463B\SOH\120723X\DEL)]Ci;^\990644QNY|\21934+=IvL}Cs(XZ\1097328\"\"`\1030751\66866\"\GS\110789\61651\&3}\\\NUL\b\SYNjXqx}K($W\DEL6\1111029,^\1062037\&6_@,6\154135\58793\v0k\EMss*KU~0{u\1057108\50460O.bj[s.\fc'\163093$+9}6N\1005809\t\DC2\v\18009FiP^;Nf\175111p\FS\1087953~L;_b\1063524~B\194939\1054559\1071095\RS\ESC\153332\ETX\CANF[|\NULm7\ACK4\ENQ\a\DC1\154992\1017636C2\a\158515=\1070658\166080\STX\69645\FS7\US\8490[\US>K\93799\160325N?\DEL9\SUBJ\26678\tS|\f\DELs\124929Zq\1006806d\1000938UU\vy\a\1034899\DC3\EOTs_`zze+\1084574Ar`\1090382E\44935'\STX\189612\"Y\DEL2\FS;~'1\60170_S\SI\1000487\&9$]\99711\n\1089413\EOTDq\149905\1066960\18266",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "Hg=="))),
      newServiceAssets =
        [ ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing,
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)
        ],
      newServiceTags = unsafeRange (fromList [BooksTag, PhotographyTag, RatingTag])
    }

testObject_NewService_provider_6 :: NewService
testObject_NewService_provider_6 =
  NewService
    { newServiceName = Name {fromName = "~k#\f\159564\SUBj\DLE_\1092710b\1027628Jy\ETX\ETBkl\165497U\96900M~\f"},
      newServiceSummary =
        unsafeRange
          "\1001332Nm#P,\45008)AcMhv~\SO\27683\&1<nxaKTV\61083\CAN\1033111w\63505w|y\1025454<\1059993XYx<\170759)v\1081323g4K\1088792A0u*I=\b\"*vG\1039438\1086564\CAN\148304\GShv\13626\17352\10171\SYN\SOXb-YiQn",
      newServiceDescr =
        unsafeRange
          "0\tZ\162395h\1058195\182829g\RS\STXS\NUL\1104762\174579\95932\USl^\f\NAK\133394\1040759\100638*vez:\96616\tLOk\US0\1059669\NAK\174088l\ESC\STX:O\ESC\DC2\SYN\DELa\78266\987699A=Q\36372#\1012923\ENQ\11110&K\DLEppr*H*\1091389\t\1045104 'WrM;-\186112\v<N\SUB5\SO\nZ:\1035612\1008461\167610\1076116]'xo]\1004453\\\ETB\1037080\1103502\SUB\158836ia\DEL\915\1039469\v\ESC\SOHB\1065466qb\173318\SIlU\917803\985520\n3YQ\1034716\154304\1104630\98801\STXa1\141726\1063736\999326E\DC1vfFn6\v\DELck\DLE%\1104910\47328E,\1035355r\60241\&7B\182524\SO,\STXcLMW\DC3nz\ENQS\1048444L\1108026b\EM\SOHW\GSd+pj%w\USCs\SUB*\170296.A\SUB\23587\&7S\DEL\1095787\138948F\1085386`H(7.\1011098\&1\DC4\38084\NAKO\1047024\1088019\148901Z\141898\"t\DC3cx\GS\ETXAbO\SOH\183337BA\SOJ<C\31113\1095009=\r^\NULoW\t\ETB\fS5A\1099141\42340~p\b\25281\1081492/\1099429\34795WS\DLE>\996173\171834\1074102\182207^2J&X\1044436\SYN39\n\996430-?\DC2s\nAgK\189453\164395\SOH\28916\1076636\v S2\60546R\SI\fGH\1017314vG s\RS\DLE#|-\EOT+B\1056730>\ETX\1010813dG\SUBAI3\1042551b\n\168494\ESC\1111323\1076090\ACK\t\187330\b\5081`\97922zPC\SI\35029$\DC4\43007M39\NAK$\v<\"T>j\987204{-\1076876\STX\f\tc\DC3L\989671\1046898\b\70674O\SYNhu\171523G\26288\&3l\RS\100843;oqC\1052062y@8WLY%\b|%<%|\EOT\n\1051476\DC2j\111001g^\GS\1074269D\26078\1059347!{\149022\5595c\42379^B\150328h\rv\vX4\45384\4666^\177230a)\DC3q\NAK\EM>\149350hgm>\ETBosO\1078758\DC3>B\RS\SYN'\1046709\1105590vqW\993841}~\19286L\"8?\rX\EM+\1029059\162865<w8XQ\1066448i\DC2|R\990864\171509sx\131887e\f\EOT\fd 04\11853 e\v\1100734M\EOT\1030466*pG\f\r\16991,\SOH\154755oc\996056v\US\984230\1042664d\12174g}\184796\1033509:R\1088470\SOY\DC3:\ACK\1008028\1027747\1078084\1027790%h.\176849\SI\tu\ESC\STXS(NK2P~\137477]\100220\n\992636|\"\SUBH\175817\1085916&\987397sB\189315k*\51713e\1030012\1054259\SOHA)K\187844\78638\ACKn'u}\23709>_msT\DLEC\vP\987887vK\NUL\148006\94836<\nPs\181406\DC4 mC\134539b\165417\EM\ETB9w\ENQ\t\125023h{af6\62917l\1031196[]\FS\119204\1093135\152421!FF0(+\GS\138939\DC4(\\ \a3D\1062346\989776\ETX\1074612\1067542\67234\48977!\1095020\1071201\&7\r\1044514,'\64378r\18928,\30100]\ETX]g\1068722%\v1PaN|\29731\\9>q\1032074D2M\CANH~\136362F\GSh.\137987b\1093136`hj\1043191}\v;\ETB\120896\&9ag'\1045769\42338w|1\ENQKQF\ENQ\180290bPlTg\1000893\NULhs&Xp\159695|O\FS\ESC]\12451\1004259\1109268\ESCB\SI\DLEs>>\166546\STX\66760\DLE\23556\1015222ufr*%6XV\70347\&43p!MKyD\100523F\1070581hl\1108731[\"\rf\1065051\39202\995985K\1012063\1075466Kj\ETB\SOH\DC4w\993379\64755_>\vq4u8w\SUBDsv\SUB\1034849\DC3b \a:\CAN\185293%\ETX\EM\999633i~J&0+b\ACK+\14559_\SYN5|\v\1044711!qa\11323q\163961|?e;K|\r\SOH}\1036310u*\1016172!8\1013791\149106<\96204\FSt\f<\4245.\159350e/H\NAK\DC4\1030768\rC)\DC1\1088669\DC4wV\1019563\n\177343I\71865D\SOH2LEQ\1105586\33663\15553\EOT\1061063\30393\13469\&5\21151=qQ\SOH`\138898\US\SOT\97592\ESC \SOHn\SO\&H\29261ks\ESC\133568\13391eN",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Nothing,
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing],
      newServiceTags = unsafeRange (fromList [SocialTag])
    }

testObject_NewService_provider_7 :: NewService
testObject_NewService_provider_7 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "\997165\STX?\ETXyo4=\STX\135055(C{\1036102\59264\ACK\STX\1112104Hm]\1028558P+\DC3\GS9$\30809AZ=\5767I=3\71908{\12608Jy\RS$,f;s\DC2>\\K\NUL\11322\168038\b/\1090778&dhMp\1092065\32048wI\STX\125061O\EOT9\1061998qoW\ETB00\141706|:G~o#\SI\1090538;`(\1089570>\1044779M\1111824W\EMA\183602\SYN\1004936x\1013229\SYNy\r\DC4^A8\183729\f-\1061269>"
          },
      newServiceSummary =
        unsafeRange
          "\18821;o\183443\&6\40840\"\\\1034473Q|\1064800EZ'\n\178647+Z\987593mp*\917856\1095363\1026197\SO\EMz\993369Ga\SIZcp\990420/\fM\\4\1893\&0/F\FSh\SI$\33431o <[5\NAK\1032486\SO:m2\STX\1045869\&8%\53669X\1066400\ESC?\121199\&3J\27987P\1004829\SOH'=o\41354)JC\1000230|\119056\153694\1075047\174254",
      newServiceDescr =
        unsafeRange
          "MDL^k\71091\&4\nN\ENQ\125244\n\14824Ls\1025603\ETBZ\995444\1080606%s_\52945e ,fu_Uh\v~\DC1i{X\1066272\997320y;\146091\SO7:T\a5.\15595\53851\1091105\&5!9~MU\SYNhe_\1029424\\\1068425\47356f\175975j\996647)\EMYT(P\FSH\EOT.\1022673(\DC4v\40229\&0\42229(pO\1023688R\USHH\1042051\ETX2&Qr\DC2a\1082013?\NAK\EOT\ENQY\1098489\DC4lc\DC2\EOTV\SUB\ACK\DC2\DEL\DLE}'{!6\984622l\25911\97712\GS|\29534\rn\11607\1058483\52971O\1011758()\1065785L8+\162494I/\DEL t5r\SO|!T\21360\165540\1085539L&\45934\171629!e'\14018TY\1049416I:F\997136\1042840{!sH\SOHr\faNoi /\FS02n$|-k\v.\983712i\1018350\ETX\CAN[T\1070149f\43178\28137\bG\48651@\EMo\FSw\33278SB#\1054312`NM--\24709bl\NUL+(\154952\1087726i\1092512t1\DC2\FS\48082h \57612v0-\r\b\54432\1082310>\v\21758\16225&\1099274\FSn\57636N\58185\992122\&8\1089470\159587\136857v\CAN[\28037ka\ESC\USa\1078972\&6\CAN\EOT)):E\RS\vn\DC2\DC4^\US.C%v`HuL\DC47\"\va5\r\989144\163130\155081Z3\ETB\133072\CAN.C-\1084366f\"\987126\USR\ACK\SOH\1006977e\1056016\UShow.\NAK\GS%=\1064359 l\1033043\DC3+J|C\bbw\989831\&6#\983793@ FP\1079611\DC2\"p\ETBSC\DLE\39654B\186605ry\FS\FSf\98552\1080150\174678w\RS\SYN\GS%nw\DELl\1025096\1014501MFjF\186592\RSHa5\1040865\45068\&7^L\SOHl@r+;\53332\r,H\61394~tL$\DC3l!jM\SYNw;CV\bC\DEL\STX<\188438\159477\1022052k\37268l\1000280LIy\189623gy>3k\1011879\1109644\128863\149356'\RS$^9\37447}3\DC4s\DC4Q$_|c\CAN*\ESC\1033147\"m?3f~\t&xc-5\ESC\163264rL\DC4\135776\nr\42477\f\DEL$\1028841n/9\GS\1022588hh\DC1\30025\1041632\14005E1\36574kI+\1111814\SIh/\"b4Fh&\1037591F$jJuG\EM\176642\DC2`LS\SYN\\N^\SYN.\ENQ\1020150\1069140lJ\991251[t\DC3#mo\9435\988926\ETBE%[0\208l7\140082\133348\11070'\FS2+\NAK\1043093\989064L\135281Fsn\47926\13899!&N&\ENQ\1814/sM\SO@\bkQ\1050855sU\164521z2L\RSw\t\DC48\42300)K\EOTj\986766EZk)\137801\1019430\1011027p\1090829\DC2\ETX~yR\1027446/|l\158987\v\65140y\SUB\SO\SUB\CAN\1015407\&6\167844Ep\1072351Td9L-Tz$^\71196\158498L \1089164oG\SUB\142561E'\23794\DEL\SOHH\993732MeIm\1096691\1067006-\1064790k0\51119\b\29365r\1083448R4\CAN\EOT\1091487\121066\1051116>\NUL?Nb\rh\1053795u\DC1=O`\FS\98379\1051557&\CAN4\1007290\83068\141118xtX^xw\SI\34263b\184367\1051118\1006271\DC4\\H\1047674ILgKn\132611V8y2\NAKgW\\p\68179\34736t-\SOH?.t\54674\1005259AG6?\DC1JPy",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Nothing,
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      newServiceTags = unsafeRange (fromList [ShoppingTag, TutorialTag, VideoTag])
    }

testObject_NewService_provider_8 :: NewService
testObject_NewService_provider_8 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "k?i\1008346M\FS\DLE;\986574\1091962\SUB\b\16140c\GS\186110z\EOT*&\t\22996\ETBXr'i.\RS\158926OM\t\64914&C\\>\DC2}3\"U\11442&n\"_\1068484Km\133812\23049Z\1002631e\44651"
          },
      newServiceSummary = unsafeRange "\ESC0/",
      newServiceDescr =
        unsafeRange
          "\1059458 M\FS\174846\27058\FS\36516a9mN\NAK-M\1057660N7\1024672\NULm8|\rq\78558:\42376JI\1044677\1067213\989237bSV\46439B\r\58471",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "tw=="))),
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      newServiceTags = unsafeRange (fromList [EducationTag, GraphicsTag])
    }

testObject_NewService_provider_9 :: NewService
testObject_NewService_provider_9 =
  NewService
    { newServiceName =
        Name {fromName = "\153308<jc4x\78856\ACKx\178368\&7\"\aV<\afN\1095288iX5o\1079875:\1078669\6975'oe"},
      newServiceSummary =
        unsafeRange
          "+\fUv\166644Y\147187v%\995809a\ETB\131089\144465\1044800\169924~\NAK=C\1023746*'\n\CAN\ESC,\164520\12115\36157\31139\DC3\ETX*c\DEL\DLE\159863\989406\NULU\9949u",
      newServiceDescr =
        unsafeRange
          "uH\1023235\48239\41433K\ENQ\tF\DC3E\164643\1023519CQ\40653N\ETX\1108119\1018754\1036425\138738<\GSO\r\1101069kI\29525zR\a\1036477\1068176j\985974\SI$N\ETXyx\12843t\\Y\ESC\DEL>(|1\1049588\NAKx,c\DEL?\1052090sR\US\997566\RS7\RSrMekX\28047a\CAN\60705YR\DC46\1049648ycX\bMr\1077022\v;U'zoa\1070196@\12820\145765\SII}=t1\1059521\ENQ\1048968\\7~(=1\1107964c:\DC3\63439M3{3\13201\FS\1020933^z\RS\163003<f2\b'\75052\NUL\SIL\142754,\181434\FS6\ETB;\SOn\43037\DC1\ETX\SYNz2\990799\985224Sh\a\38125]x>m\SYN\62956\SYN.X\1095904\1028165\988795'm2!a&#\155115Q\1000764^@TW\93850\DELwX?\168236\&3\NUL\1106264\1090893/]E\ETX\ETX\78123d<Ybellf?\fGC7\1064610?\DC3Qr$\NULT\\\1003148( \EM:l\58788OaTq)6\110717z\EM}Y]\1109109\1091691\49439\1037167iC}\93806\1091152\1084621\SO\ESC\EOT\119943\36349\DC2\59934\v\1092529\&8V&\CAN\FSA\1014278!R\145224=\DLEIf&\ACK3\1067986bM9V53c\vda&\1001625(r1~6\1062723\NULj_6\ETB\10501'E\45393\n\48533#\DEL\DC4NBq\1101246x\994126\r\EM\DC3\ETX.7\ACKJ%@Wp(b\1088255\&1\USm17)U\SOH\1071086>\1102045\999348p\78883eG\183260\t\1061747\1088048\25675\&9N:%\1114072yI\EM\SOHp\tL\EOT\v.J\154709\73029\78467\RS\FS\1090599\CAN\1057095#\GS\f]Y\69245\DEL=v\EOT\ETB\996948\&5\DC2\7008\65352\SOHBvw\83234\ACKV\GS+ju+Lc\1036584.nhc5\NAKG$\94050\DC1\r\19114J\74531\1037084\DC3gfA(\v\FS\nvV*u\147221\1004743j\1054691\US\32479x\t+<\ETX<\78713&O\"\SOH\EOT\1043480\161084\&3\42380%\"h\36212X~3\1032777W\1030234\SO6\EM\178449\994369'%\1079182\1678\ENQW+m\1071833\NUL;\1108217Y\52744\DC46:x-\f\DEL\1034084%6<pa,`\160367\&7\nZ,!\160359\1109496\ETB\1038147#\1112006\1050569k\DC1\1032042\CAN.V(u\28049Z\DC3k\1039121\1054987\EM\29969\1036663\EM\ETX\1074434\&1aooV<\\3\v\SI\1014949*\5720\21940)\\j\1037615\38886KKkd\18806\998409wV%\1113161\CAN\DC2~3?\1037474\165841:\985080~Q\DC3\171837lx\SO\986651\ETB\1014527\142684J\99272F\DLE\DLEZ \98586\1104172K&\12905zKY\1102360-0\USl\a\\\a}\a\153420\&1\\+\32085Bs>i\1067129_\DEL\177902\45052<%\38813iz9(\150705@\DC4bW\r\136450#$\22059\DC1\1101674`:U\1002322\172407\51672\apt1\14173\&18\1017432}\1012555o\118798.\1106604\ESC]DO8x\43489\n\SUB\18611\133734\167799\1106886l\173726\&4di}\FSQ\NUL\f\b\1042475\&0`\\\SUBarBM\40853 \DC3\ETX\50025\STX=\SO-A\172822\1069034\t]6\ETX?B1\SO$Z\ENQ\190504\ENQ\b)\128193A\1015925\1111209\1014356\995276-uQ\DC1\1017678\&0N)\172322*\RS\b\ACK\1012537\EM\vZ=\998754N\SYN5,W\1062374\DC2m\171434\997948\DC1\\&\n\ACK8\1106162a8o\b\DELi\186019X\NULR\1028849j\CAN\28029Q\1019102\DC2\94916dn\1076122O\DC1\1010172\&1X7W9eR\59193U\1092535fUkm\999383\&8\EOTXP_>bT\DC3Dq\1091467B_o\50481P\1008711WB\ETB^\1051755\STX\EOT$8F\78605<\n{\45022|V\50257^TG\SUBjz\v\983679tgKDk)\SYN\169942SmdyXa_\a;\n\SUBO<\129383\GS_^/m7Kj\NUL",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Nothing,
      newServiceAssets = [],
      newServiceTags = unsafeRange (fromList [AudioTag, ProductivityTag])
    }

testObject_NewService_provider_10 :: NewService
testObject_NewService_provider_10 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "\GS\1110090E\SYN\ETBU\1020350w\1040125\1112645\DEL7*\175197Sb\68491\r\38208.2\vL\DEL\EMO\DC2=\SO):\NUL%\1047965\neP\EM?\DC2\nI\1095150a^;^\178884\174598\DC2\fb\69443\1017411\10869\1011993\&3\US\110828\987422\1100991\1010216h2%"
          },
      newServiceSummary =
        unsafeRange
          "\a\1010507\DC3\152514_vw\\\bWxJe\185661\162478\1035553n\t\DC1\1040174\DELM*7k\RS,#\EMoO\EM\b\168057\DEL\DC2\1008546\137752\1098328J\1010176?\164555\"\tS\ETB\DEL]PWw8\EOT\DC1\b1\RS\1028808\13445\38395XR\92608xm5\45232O\985042\DC4\1004627\RS\1037308;\FSv\1032069m\r\n\SIXl\v",
      newServiceDescr =
        unsafeRange
          "6V9H\RS\1034250\1028756|\1010414\&0\150886\45515DHs\1015582IeJ.\STX2N\41241)0_-\68182|\SUB Y'D\43701 \157644\DC2g2q|Z\1014859gb^36T\DC1\ENQ\147212_8.|\ACK\54429oQN6&o\RS\995894D#mEgg>\n9L1\ENQ\FS\EOT5o\a+-~i'\nb\150845~\US@\1092496'\DC3\SYNe\fPm\ap:\1050103\&5\ENQ.\26309\DC1XR}\1010133\999419\154309,\1040213\&0x\553\1066780q\1050432qx\1043610\1019823/[McF\1084882@\165419\DC1\EOT\1011178F\161250hHNO\1093078q%\ETB.\1021317M\135841\ENQ\135854X\180587\GSi{o\"T\54983 \SOH\53443C\1032209?Dsq=\1095326\&4SoiR\9772\1041916\1080594T\1001650)\bu\138399C\1431gd\ENQ\vT\1033346\173890Ys\1021807\&6ZNo\181513\1030695\GSJOw<HO\986455y\1109043\DEL0f~C1B\FS\CANqM\ENQW\DEL\7051\&7\SO\1034370\1103552E\1096110\139653\1001260P\ENQP\r'D,?E@\DLE\49286\&7\"Et\165275$\1108514o\60213_L2\1026359\57701h05\1034609))\1059275\n}\b\\T\vD\1016588F\119310YJ\USRI\RS\EOT\1072423\95072\21810N7\1111844t\SUB?\r7(2NcU\15263#\EM\988078\1015688\DLEs06}Ir9h\4444\txo\DC4\24054X\132169d\ESCC\US\5797\DC4\DC2\\,\133293\US\US7\141538\62504\&5S'g{\173523\1063282\EOT\167350#2JL6/ip`#c(3\EM\38208\&9\70787\178717B\1075130YBp\SUB)Tf\161877\DC2\24561 &u);Nd\DEL\1769~\62246\ETX:gTrs\128681\47070\1019647\1076809]O\1045261<\1070221\DC3D\182028f\991295\1084989)\fx\125238k2\991278Z\166459kQ\SUBLV?@Pv\1099829S\SUB5}2C0V\27900kmU\51896P\USG\b\GS\162395\11909@X\987609\&4:B\1020974\v'\NUL\t\1028314L\DC3\1073850T\DC1\bH:\1013898AiK},\1035863",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate ""))),
      newServiceAssets =
        [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      newServiceTags = unsafeRange (fromList [MediaTag, MoviesTag])
    }

testObject_NewService_provider_11 :: NewService
testObject_NewService_provider_11 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "?\DC2~\SO\SOH\120500zN\15822RM\SYN\1038604~\EOT\1021241\ETX<\14563O\95990\&5\EOTVK;\999439\992301 ;\EOTU\989956C\52124\178084\1053253z\a\1039345gi[\ETBT\15322\118975@\184040\150620\DC1\STX;+\"m\20380\1052598\1073157c\FS\CANY\SUBJ\1049581\1080786\ENQ\RS\987830&c2yY/J\RS\vx\RS\ACK\1025559\1108113\ENQJ<y\60063!\twC"
          },
      newServiceSummary =
        unsafeRange
          "}\1043869\fz1f<H\1082931H2\1097046\1069581\NAKxT'!q\1056985\997296>\68818\DLE\rD\5247\119181\ACK\1027288P6\1110446\135308\99035\ETB\"\1019335'\ESCo\ESC\25977\100852-O\\g4c9\994795,\1100228PI\b:r,\1056366@0\r}\1049947\1007809*o\119846\t,\1041336D\1008156j;DA\177118\48074\SYN7(\128822^\1031685h\129361%",
      newServiceDescr =
        unsafeRange
          "\1094490\47110f\DC3=I^f,\1027547PvgntY\n>\171218\1069439L7%\1099619E*jc\r\49776\100798W (B*\1089682;C\175938\1094295\917792\ESC- >^\131959\ESC;\DC10\64616\&8\SOHS\DEL@\1029947V;\EM\DC1[[\STX#B\1006388\1054016m\FS\DC3\ACKN\FS\1065844tY#q\1058943\95376\63951$\18779}o\CAN=\25843\173355\SI\1010579\US\NUL\RS\1021032\ESC9\NUL\a'\156403K3\DC3i Fft\GS-J\b\f\FSCL\1341\EOTBd\ACK\64900\EMNmzb%@\988167|2\DLE\168675RXH\STX.i[\EM2\1087519\&3\NUL\SIZxy\1030388\&3-s\SUBM\1048108\78771x2\1012023\SOH/T\30357\1023603p\vvl\DLEcS-\DC4cK\\\1049234D\DC2~\1055984\1107629l~y\EM\RS\ESC\150501\RSf\1078307b?\n\1036765w\1051887v\1029488\\Wt?Z5\51473\48588|\12842\&2\DLE\178673\SO\NUL\1104638)\FS\45815\\z1f\SYN\SO\139049\159241\24593\SUBN\8313:`\1063003\DEL33.FZZK\49338\t:\1018035\US_\16496G\72215+9\159443S h\174025\DEL\1010072V\DC3`i\23622{\62882u\1053590eJg\1091602\a\1028624'yx\SOH\1043659V\STX\1032570\rIc\NAK|{rS>W\\t3rc\149239O~54\1383I?FKr5\22890e\1003904>\DC4_ZS\22979\RSyYO\ENQ[M9\SO\1005595;\1076110BC[\"\SYNx\GS\72112\1028337i\70797\DC3E\29346\16868L{\CANc&3\1036449\SI9\1103622\21068\1003881\ESC\fbf\1088410t\US\v'}\SIv\DC1<^P\1007704)\"\1090374'3CH&\126124\NUL\1061859\&7$_\58749\92235\ACK'\SO\41124\62025`S6v\ETB\1093195\98281\DC3DGX~\66908f\USj\96422\1055714\1067406d\ENQ\123144YDm\1111312kw\DLE\f\ENQ1\41731\ETB~|\1090320\51022\1111927$h\1013133\99324j\30158\&0;\135887\59005\STX,\17969\51313U \tg\40004\f\131172\&0\169112n\NULMw\1060544|F\1098330\69770\&2lh\DC4\EMW*\1008702!]\ACK\DEL\v\ACK|k\EM|>\DC2\STXkszV\1095699v6\NUL\DC1%~\1089593\ETBZ\USR\1029237\DC4\119005{\1013788~]\ry'\1060054C\990263jD8;\ENQ\rG'\185547\ETB\b\1100859]\ETX(\162884\&1U~:{\RS.T\984882\DEL\EOTaI\1027034\ENQV\US\SOHw=g\149454\DLE2\994319\SOQd6w!A`~m?rb\189789o$\83364(aDS\136252\32471\1017782# O6\DC2!\DC3w\1109737\164572OR\52442{w]\23716\US\b}\SO|:=K5X\SOHz\n\STX\100749p\1090721\1045799\&4\NAK\1014854\1056821Tx\21953E\DC2\fQ\ACK\1038428;\41543\12188:\159187\NAK\64701\&3\33301\1058478i\DC4\27247\166154#\1090956Uo\r\ENQ$\NAK\SYNWF\b\1066208\&6\1061734\EM*\b\ACK\1013089?V)t\144940\996075\n\1098429H`$\ESCt}[\a>sf\1112067\&1\30412Wk0\ENQu\DLE\1021481p\183144%\\\68410\1059355KQ\ESC?H\v\1088920\1074016\&9\128955E\136257Q9y\SOHCj\t\140844\78469wB\DC3y\DC4\1083934\1029565\139260\2597\r\23862\STX{Fo\169894e\172547,8\a/\DEL\SO\1012359\DLEP5RZc5\167147Z\FS\1003759q9W\"~\NAKDz. \1047636\&4\1105909B\1030822\NUL%J\158515Dy\990985/h\DC1;\189733~dhH\\s\1025368\nBPyqm\US\1076951\51643?{\EMTa\49050\GSD\STX!.S\177656\EOT,\1106010\158764\1052713\&82\1058147V\1080610^uw\f#\1103146NZ\DC2\1027932\756/\b\DEL<\984915\rh<\DC3MkO\1081994\166564K\1004827K[\34959\63599y\CAN\110729y&\ETB9,Ca\FS`XF\r>@\177013[\DC4s\EOT\1101117\rRW\DC3\1067719}UtM,D@\t\GS\1016990\DC2BKthM\DC2\vH\984151\52434!\SI$T0IUP\t\DC16k{C#\RS\9757\US\1005025+-\1006128#\1104179\SI\SYNT,[\3365\172037KY&B\vi\DC1:\179615S\1056679\SOQU\6409d\"Qm3%\999468!vE{\100022\1024498v4\12809PHf4\1049121\984182\STXUD\18320Jo",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate ""))),
      newServiceAssets =
        [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)],
      newServiceTags = unsafeRange (fromList [GamesTag, IntegrationTag])
    }

testObject_NewService_provider_12 :: NewService
testObject_NewService_provider_12 =
  NewService
    { newServiceName = Name {fromName = "\tq\8503\SIOm\SOg1\1048749}\174401\EOTM\\\187674;l"},
      newServiceSummary =
        unsafeRange
          "\10509\986962\140895\SIm7\1017365AoM\"9\1027617Jd \998522NA\DC2\95597\DC4u\154917\1062158X;\USBj\EM\1058291\&3z\SUB$\1076634S\169432\1036368%m\135320\1085520M\DC2\62890\164810\1038020T\1025106}}c\DC2\1019026\49943\132437\RS\SYN\1033703`\r\986287b\v\1090310\af\17059|y)}\ETBoT\1036457JInY\SOC\131634G\143454\1090616qfn\DC1\CANy\1101878.\1106980JQ&\SUB\SO\14988,",
      newServiceDescr =
        unsafeRange
          "\SYN\168165\SO+Fp\111328UXh\DEL\GS\1095336\4098>\183345\57818\f\42753\137538r7K\169055j8MUbx\1075327P~\f\1017462\998967\&94f\1061145\ve*N.e\13097\&9}\15756'\SO\1102194yC\139725UN\GS\\\\\"@\"\1002235T|&\bi\1033476\&0\SOA\170936\ETXr\ETBEo:\ETB|8\145691\1059074-\SOH\1109238\DLE\190737\186438L\ACK\1111044\DEL\DC4\SUB\185580\aDI$\\faHndk I9\1095025\EOTI\57498]\27718\DEL \1103710\1022018Ot\131525n\USgdy<25\ETX\96205%\DC3 \163442\182888X?&2\995763]/\182983k\1044278W\DLE\ACK\b\1082388\31701\1001625Z\USC\tN/\983716{\68217\DC4\ENQ!\1063824K\1109721\NUL\118924RLr\166302 \SO\177079o\ESCz\f2!mg\ESC&\"ESG8=J\63361\&1\ETB?XChkj\73821\ACK\aV\ACKA3\157664\45697t\95813t\150490:_\150472\143314>\b\14898\DC3J\54454\1039203\1059989\FS)\r\9229\ENQ\69445\1101478\NUL]\1100319\&1\45210\FSU\25284\13396!Hd\EOTTe\1092709Elg\n/\44002\f\39193\175828^\STXHEj\US\ACK\992335_\1020650\&4j\1075366\185347\&5\168610kgP\SUB\31112dFG\994985\STXn\1038272W:\1062436\&0\170912)-8\3143Y&[\tn\1071565\1082608I-b\RSV1x\ENQ\1098525s]\135421\74146\SUB<st\a~\8952M\184929^im\GSFM#\58631\163237s\USM\tI\GS\44582\15472x\ACK(22\NAK\vs\173051+\1044609\97759ma\1014187>{\135207\&9 \a\5784\SYN\EOT\996836-\1100071[D\1069370\SYN",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "aXY="))),
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing],
      newServiceTags = unsafeRange (fromList [SportsTag, WeatherTag])
    }

testObject_NewService_provider_13 :: NewService
testObject_NewService_provider_13 =
  NewService
    { newServiceName = Name {fromName = "\SOH%\SOHx\1260"},
      newServiceSummary =
        unsafeRange
          "\1070116\CAN\137224\1039335\97572\SOH\DC4\DC3\ACK|fVa\v!<\1083043\168179<\\PI+y3}\121426\1039816\47071\US\vz=C?\1056614ry\178791\179999\1022502[\1025185\1029363';mnM\39259t;IT{G\68086\15922\SO\1047651~\169766?*]ntB&\ACKD<x\STX:W",
      newServiceDescr =
        unsafeRange
          "8\bZ?\DC3\1094658W.\DC3B/\b\159039`x+dBT1KQ%\SUBbu\96233\EOT?Nu\1063411\&8\16837\1040911c*\SUBHAE\121037\NUL.\188236|\183650\1107486\11918\&9g\168150\14686\US\ETBN%:j\1091910\DC2-\NAK\1070008ZEw*\DC1\128298\191134L]\127763\ax /v\ENQb\NAK$X \1082504oT.H\EOT\b<n\v\DLE\1020516\25065K\US1!\STX/M0v}\169723\&8m.u']\NUL\64931\v8!\985766Wtu\162405\62019rs\ENQ\ENQ=bF1A\US1\EM\1042640\128226\147315\134018\1072123\155490E\SO*\SYNgr>K=vGF\1039102k\1092267\139831\1050004U\SOH\"w\1049107.jf\62545\144360\&5\126593xKP}pj\DLE\100265\\Ya\1075686\1052834\33937\a`*?Eel4\EOT\77917\25595.\r\48343\SO:s\DC1\98822%\14182\SOH..\149867&\187511\RS\1020997O37\47084\SYNnE2)G\EM`\60841&'\1006693CAD\ACKa\\\45893ig?y\1082692\"]-wC\994911/&J\1079827S\3203\1039995g+6]'oP*\33729*\1041637M\39836\157107z\1067900\145614xQ2Y,(\170968\DC1\ENQv\DC3|-\1003511\"GL`\1036906\78252\NAK\1022510U\39634\DC20\1073180\US\STX\998305GTs\143070(~} abCYS\998309\1072665BW\1094023i\6097\157803\144940\173321\&0>#\1101516k\135328\168154-\SI\188752_f\998539\v/[%t\FS;HX\174842\1107063\2421\137803\vIH\139544\&6\DC2\NAKL\161099\172988O\1111941(J4\58949p]\146697\24516N}s\14848rx\9380\1033797\GSII\SYN(Zweu\1015457b\1072908\176844t.xMfO\a~pTA\US\1009167DO\31455Od\1001112L<}b6RM~\1099509\149783\183826\1060322\FS\190068\164438\1043495\1078522\174082THsF'\173454\&8o\EOT",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "qH0="))),
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)],
      newServiceTags = unsafeRange (fromList [FinanceTag])
    }

testObject_NewService_provider_14 :: NewService
testObject_NewService_provider_14 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "\1069625]\52381)\t^<%3g\1077277\SOm\1088813w\1096207\ESC\1019827=\65448\1073722\177874^\179370s@\144950{IX\ESC!\SIk[\48330\SI!"
          },
      newServiceSummary =
        unsafeRange
          "\ACK\DC3\12186\r\1041728pl+\DC2{\"\SYN'FDzP\DC3n\157725_\r:$4<\128981\&0\ETX[\ESC&\b\EM6\DC2\157160xhvG#\DC4\ENQ\1048628",
      newServiceDescr =
        unsafeRange
          "Z]\1047600n\33103v!\ENQ\GS\183601h\26114\29264\RS>uz\1031071%br\ENQ`\RS\132845\DC1\1058563c-\SI\DC4i)\"jWZ:\FSn\EMH4\DEL\1084465\&0\DC1\n\RSx{V\62013\182049\&3g\1045503\1107756e\1035249]\188119\1074323\163233~\1084780\ETX;b\1075088/\50353\&0\1016034decuih\n\46807\14828\998955\v\141720\1011586\1014665\150995\145217\158479`2w\a\1046087OlM7\1030173\9120-\1085618\165058.\990259g\USL6\RSS\SYNW>E'<v\DC1\120167}a5\DC1\137400\SOP>/j|\113783Ks\155177)O\1015803\ETX\142708\DC20sQh\54858'.P\DC3\1000750\655\43907B\1073883O\DC3]j\EMY\99174\STX\1067903DO\STXa\1094741puV\DEL#\1020263k\n\111137^(\f\1016733\1030137_\v[,/&i\DC4\NUL&x\DLE\b%f4d\ETX\rI\47203\\1\182862:(w_V\US\992573\&8\129111\17325!D\v\STX\RS\1001525C\US\1048403O\bY\997316}FB\1015425 kC\1049688\&2\1025454\GS\13148x\1017908\DC4\ACK\34867\DC1\SO\1019176\1158IePP0\SUB\FS-\NAK|:za\43264\162118F\NAK\CAN\1077956=\11814\138676\&1\DC1\"e\42909<*\CAN\DC2\186614\SOH[\b \152269uQjx\ETB\156521\GSR|\ETXc",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "ukk="))),
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)],
      newServiceTags = unsafeRange (fromList [PhotographyTag, SportsTag])
    }

testObject_NewService_provider_15 :: NewService
testObject_NewService_provider_15 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "\SYNCg\1050885_\1033584L\v\SI_i\DC3\t\126570M\1013051\983745\71221x\132262i\STXLy\1036476\1080908\1105734\1097101p\SI\54062\DELwk!WT7)\ACK8@K\93814d\EM\"\1001565YR\ttX'\b\1024898-'\135803\t\7099\&4[P>\ETB'+\1064307\42232T\\\EOT\1012466\FS\SOHdO,t\1033839\NUL{WQ\9714r\ESC\27164%b6!a\FS>\SUBs\a\nb6mC\ESC"
          },
      newServiceSummary =
        unsafeRange
          "\1021699\&8UyY2\DC33qr\1005027\164345\1084432\1104189\1051093\n\158223\&7lx:l\150490\SIV\GS j\987582\&0p73\184216m\1037346\n=\DC2 Y\998768\22960w\1033881&7$eD\189989\&0",
      newServiceDescr =
        unsafeRange
          "r3j\47447x\\\998966\SI-p4%'6j\US\STX\1111906\DC4\ACK{\NAKCs\v\DC1\\\1021466\24496\1042909\a\1103300'\RSmJ$\DC2\NAK3\vgi\v\1014164DnY+%j\ENQ\1072664+e=\1113559\52883\&1\1000330UY\SOH\1029835z6!(\t\1055563\SI\t1\1074727E;Z8\1007751\45850 \ENQ(\ETBvU\GS\1063274\1040550\t\1026443\GSYy\42580\v\141785\DC4c\135978Yf\4038\24450\DC1T\t\170197L\DLE\v\153960\12375\156037*\987760d\b\22859\&7wDK\DLE\1018613)\1087838o\25450\EOT\EM\1101963\66237l\FS\997983`\41931\1033412'\1068325JM^2\ENQK\DLEw'H\179407F*\EOTSt\US\GS\74017\998690b)\DC2B/\SO\50459i\996619FI9\DLEpN\b\9688P\DC3x\1111755\74127\14241R\67632\160064:e\178771vRwQx\29138\159599\SOHJ\ESC:G\b\128369\54980\1083714M\SUB\v,0c?\1046409\3483#gY@\EOTZ\21723R\39410\92484\US1Vd\1106417\158734\EOTu\1036849ar\1006751\1066993\72716\a\SYN'\131090\1080856\&1v\ETBnf\STX\1012431&\1105179f  \SYN\171050u\vMo\797L\"~\54837d\29425C=\1010371\1031921(<j\SOHK=7\1051229@c\1023646/Gc6\188735\SYNB\DLE+2jbB\NULuE\1080045\64948\SOH\SOC\EOT\1036068\70313Y\ETX\40721-\STX\b\22187 X\999565}\1040233'\1077076s`\DLEX\47598\ETX@#;\EOT\983520\13639Lg\120653\DLE\RS\163123\1060401|\1045068\SOHo@\1058421R/x\US\EM U\f\b+\1071046\997430\STXv\US\152687\&7\1079384\62901\&6h\22386\1105295\SI\1070371p !\1054167Pm\nu\b\164482q\1052410:\36723x_\167801\b\177569\1041150\27184V\1058258D2s\ETB\22856\1109206\62446!\SIA\1027722z\1094622\vXj}\1575_\STX\ACK\EOT-\RSZ`X\31947w\1057379\168218\tQ\164307ZaUyM\121233J\94813*\SYN\EOT7\a\DC1Y5\1028707\&4/\29135M\140094\DC2B\1093254\20714TF%\1035281\175829BI=~\138663\2918n4^r\44303\r\139030*\987293\ETXS\ETXn\SUB|=\1058882A\1049912\23742\148384\1030925F5o1'\rD\1040024\42205\987569\&7\SO/\167059qd\FSP,\157828$\140206{Hr\1031373\1081052C.\EOT+gu,i`\855sg\191030!;\FS\47562\DLEi\SI\1065010\1101869EKLr\DC1ktcZ[\SYNt?\RS\188934\SI!\12570g\RSA\13755W\993905'+\1108861\NUL\GS]x\EOT#\t\19465G%%-Z\27682\186888\1112192r\147671y\1047326T<L9\ESC\190099e\173012u\138483)\1030683\STX12I\DLE\135149g\51143`A9\1018247\151528 'S|*\1047451B`u,(\CANY\re\31274iZe\95717\DC3\SYN\1085181&\1034492R\127886\ENQ~\SUBc\1069677\SOH:,=4?\ACK_\US\30646\EOT49l`\v\GS\159002\ETX\1027153\1074957wFo}^\73076\v]pB\EM\1069645$\ESC\151840?q\160029m1\15169Own\1056997\nGzII\25776\ETX\985828W/X/\EM\ETB/\1008606\159966T?E5\a:\DC4s)OC\SYN\STXX\179972/,\131232\ETBh\1059679Qy\9497XM\152339\174404pY(T\ETX'\ETBC\1091213\1022567U\ESC\STX1\183359\SYN\SO\\a#!e\1013643G-*\DLE\t$\EOT\NAKwQMZ\n\152144\54708Zu\50190\1064210{jt\1062705\b1:V\SOHPx\1017950\40265Q\61736/kO\ESCa\t\183098{n\36037\95792|+\58498q\95227\RS\156369D\160219!\DC3b\1070214\28079$l:JUm$\EOTY\177393\1050551\&1\997840\DEL~\DC1ncK\a9\CANbh%\1076206\1037862z#\987993\&7Z\1106646\1101765\DC2\1023516#\1083645(\40972(\146437]\62560aC~+p,yT]\155695\1073012?\993851%\140268\FSU*P$K>3\DC1`\DC36'`xrB\1012068\143855\31363]\61665Ja\\HJ\ETBt\r4\SYNF\NULUY\EM\n@N]B\157750\&8<\190686\SIQgcy\bpQ5\SI\SOHny\\#\159308\986887\NUL\1105314\1038153\140882\&5\20717k #\30108G4\CAN\8908&8sC,>\11251b\65926\1023791\ESC\EM\162708\148075ZRs\73802\&6\1038581~\4596\135027\10056\DC3szb1a%Mt\986098\1077690b\1051621\DC30",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate ""))),
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)],
      newServiceTags = unsafeRange (fromList [EntertainmentTag, ProductivityTag, VideoTag])
    }

testObject_NewService_provider_16 :: NewService
testObject_NewService_provider_16 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "s)2\1104782\DC2\152080\FS\f;\SUB`^,(%bf4\f\1045277d\6051[;-\1112682-Z(\f\1581\991984CaF\1109169\163215\62712\1096389B\158731GZ\164418Bdm/^\FStndbbP\1086144\1052836;\ACK6*o)d\27733\ETX\1063269\b$OP\1077881\95690eE{}F\SUBLP\36468\&3Zb5O}Ezq\bk\1015500\&8=<\1014630MD\DC2=\7073\1089336\1038133\t\1011617\38594\1030669t\fh\1054645O\ACK\NUL\41596,(:q\65426"
          },
      newServiceSummary =
        unsafeRange
          "\190917\DEL\SI5Jk\FS0M\1099308\1054673o@\44767\nH2miIPd\b\"\1012236w*\1050370\DC3Cmv6\b\STX\a5@!l\37970\ETBh:\182676\31232xZ\1020079s\60919Sb\DC4\DC2\1098520&P\147936S\b:k\159478\44775D@\n\100496ru\99051\163047\DEL\ETBX\SOH\EM> \1093728_X\\\1074836\187512(4\1032662\39341\ACKe\1059130\186051&\18240\v2d\ACKw\SI\31270`\DEL&\1093979N",
      newServiceDescr =
        unsafeRange
          "fldm&,\EOT\1090046]Y\DLEuH\1017971Vz\1032250\37166B\1050156ZvcsGQ\"\ESCn\DC3Mm\STX\994602`n\n\SYNR\1111233\b\v\b1b*\1054449\r\1092061jDi'\97919Bd\DC4!.k`p\\\DC4\146438\SUB<\1075118['\EOT%\SOHa6\r\31652,7\1032719\&3",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "YA=="))),
      newServiceAssets =
        [ ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing,
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview),
          ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetComplete)
        ],
      newServiceTags = unsafeRange (fromList [MedicalTag, PhotographyTag, SportsTag])
    }

testObject_NewService_provider_17 :: NewService
testObject_NewService_provider_17 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "=\157343\129370\DC4\1001955n\27978j\STX\1033357n\96483\ACK\1085643\1050799t\48886\1067700z.!\RS\v?\991624z=)xZ\42741\ACK\29249\RS\NAK*\164028N[\168396e3>\v;\RS\94938C\172528\\,\CAN\2755R\ENQ{\127013nj(<S+1OY\26690\179325"
          },
      newServiceSummary =
        unsafeRange
          "hU/\1092724;\995992\1028632\1076656\66282a\992241d\1058006l(Z\35936\SO\DEL\1022024i\SUB\13808`6\179480\994340\6966",
      newServiceDescr =
        unsafeRange
          "\1022032\51789\&7397et\132361;&+9\1091711g\CAN\69722\ACK\DEL\"7\SUB\47554F\ETX\\>\CAN\rt`\DC4m}d\1045455,\134883N+S\NAK`\f\997826q\36040\EME\150406Y>\SOH\ETX*<3\1072899jm\ENQ8\1041144kl\27733\DC4\DC1,\1015321\40115\EOTD_2&\1032446\NUL{3IhLSG\FS\127010\DC2M3~\DC2A\94318vo=4\1004589iize.z%P\1020282&\1032224\DC4_H\158677\&6\SI\t%\1017385Q,o\ESCKn5\1059285BPc\998628\1099059\SUBWW\158907\&6Y,W\156192\"GhCro-\48969b\CANE\ETBZ/CX}\998429\&4b\1080154\DLE[\ENQ\40089:\1004758?w\25711iP\r9Z@\132990m\1003773a\1081328n;pY@\\JB._\1102899\1024819@\148305\&1\NUL\SOHHH}\1096447R\8345\1004258H\SIxP&\nY%\SO&\100912!\992143\170486\1089185s\187025T\998762)\1079652M\990225\7013\161906\&9\ETX\tp\158012,H\rjY\v;\2301\4419y+9\73002\GS5Lyv\DC2\b,>\1080863)\1094288.c\1101988\DC14\1022642i\rf%\1083532\a\1074355\ETXSjl\43092Zy\SOHd\ACKq\NUL|-o&;l3|3\1023479B\178485^\62593\DLE\1024231dB\138390\EOT&.Pa\RS\n\NULZ\15783\33829\SOHZ\988544dN\166837F4G\EOTu'{\52909\1090580\GS\1036032\1078202\22080F\r~\a\\qF\1030582l\RSI\94495'\20634T+\184963Q 3\DC3\36700:.3Pc=\1058582>\8533zS\STX\1064269H\r8i<\DC15\146867\n6I\97905\141580MK\DC4&)\1089624I\1113522\&6J\1046701Q-h\132050\99845\ETBZ\v\tOAYb]z~\ESCI\ACK\1063237\1049032(\153975(W[\DC2y\993607\\0\179033\&3\1009813\ETB\989585\r\163438\1027069,P\94720\EOTG\DLE\169803f9BO\16333|4\153666\DC3S\7434\&57C/6\1110890F\NULn!'^b\SO\10673u\985725khm}e\23239e'\136818\132656\f\1025376\ETX\70278(v\EOT\RSBH;$U2\t$^\155455t\SO\\QL\27463u\1010353z$V0-1\95822\ACKLAs\1002835)\1065417\&9/\NAK4\1073872\1002557zn*\NAK\GS](\nC#\30105\EOT#L\DELz",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Nothing,
      newServiceAssets = [],
      newServiceTags = unsafeRange (fromList [MediaTag, WeatherTag])
    }

testObject_NewService_provider_18 :: NewService
testObject_NewService_provider_18 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "Z\72817#x\DC3\ENQu\153441~d\1610\120307\1046149w\STX\ESCk\DC4 ESQ!+-G8\fX\rU\136637\SUB\vnF\19795 \SUBF\1045437\1068888"
          },
      newServiceSummary =
        unsafeRange
          "\100742Y{)\996202DB\609v/\RS0q\DC2Df\1007307\GS\DC13#\SUBF\1069012M1I\DLE=q\DC1>ev7\SOH}<J:U\DC1?]@nv#o\157640\147158_\29733\FSem\DC1I0\170253,c\96455at\1009875c\1112341\42911c\19746U(lmd(JhK\t\ENQ\1004083e",
      newServiceDescr =
        unsafeRange
          "@\136379\144579\988699Ttt\NAKr_\1093844\CAN\r(G\DLE_\992777\f\132815g|/\EM\DC1Q\NAK]\v\1062617.~\162407\182374\163324\DC2\SYN\1053004\147415'!xfJWN\1050135\54880#*P<V\n+\96696\189627G_>Y\GS\48677\97490I;\4268\98228.ku\27548)3B\ETX\1051474\ACK\1083927h\97284\DC2\\a,D\ENQ\GS\1002119\GS\175740^3\47370,@\ETB\159910z\12079f$\1051087\DC3E\1082916R(PZBg/6tc\1104524qx\1085314\66817\DELYp\ESC3E(\1016154(\166014b\128457X\1110136${2P\ETX\155790%\1051357\37344#&J\GSQl\92706$f\"V\1023194<Ew\1048406\SUB\1081920\141722iK~\37804|Tu\1097079\ESCe\1081525l\FS\1091760\1046617\CANa{^\181451FB ]\NULHn\16933\"{H:y-W\n\137417\65859h(DX,2\SI\1049129G\SYNe\DELP\\J\991724 ~\\v\189230\70165<\153813 X3\f\SYN@\33544\1063457&)x\10940w\149280Y\1036297\183600\138654\ACK]<{U,Kp\EOT\FS]\1055668\NAK\ENQ\t",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Nothing,
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing],
      newServiceTags = unsafeRange (fromList [FoodDrinkTag, PollTag, QuizTag])
    }

testObject_NewService_provider_19 :: NewService
testObject_NewService_provider_19 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "\CAN=\1061084]RY\1064117w>\SI\n\16401\ETX\DEL=\1092443(M\bJy\1021044\a\61075rv\r5\6538\1066804\&6\1070435\128420\39200\39305\v\NAKmH\164268MO\73049u\US\47081U\983231\1055781\EM\1048893^\SO%\US\98042\14822}C\990058Av\DLE]|7:\168426 \984955\r+\1071560\64413\&7\FSP\995848\1007128Y\NAK<\ETB\68820Y\1063254\rJ\FSD\1098456\np\133333\1037862\b\161794\DC2)R\RS\1858\1000824,Uxv\v:EnAS\10616hLo\NAK\164528\DC2\94289~\ENQ"
          },
      newServiceSummary =
        unsafeRange
          "\151730&Q\990534N\NAK13xU\1024603\ff}\SYNz\ACK\190325\RS\US\120989\1109230\1047100?\989672#+\1037411qiH\61962E\148240%t\\:zBooW\1008420\1060561\GS\173169\33003i\SO\SO\181624p\SYNbZ$\1016414v\1041414\US?\32466\ETX7om~!\NUL>s\NAK\97182\FS\1001384\159049\161959EN\ETB\92479\151354\&7\1032017\b\129393K^\NAK\bb(j\1087734/~%#\SUB\58319\1083962\NAK\1096139q\SO\n\994366\CANj/#96\134170\bu8qv\USo\142673\FS\1095869",
      newServiceDescr =
        unsafeRange
          "\DLE\ETB\"8\1058763y\DLEC\1020250\1036804\983100\DELD@juy\SOk\1061198e\nl\127306\1090411\1004864\&0\ESC\NUL9Ag_.\1089442;4B^h\STX\ESC\991464\133499\SUB}\EM\DLE\f|mJYa9`Y\131890\999003\SOLb\156033\DELVg\SI}{\29255i?{\"cJIl\152485\&7\152629\FS`\1058526t\10247K\\k\US\SYN$@K \1106285W\EM\1082497\SYN\63279}0&\STX\ETX\1038174,\ETBDuIu$\148518\1101078=\1012730\f?\1110483{\CAN\ESC/L\172277Z\EMs'\134251\DC1\143548p\35113}m\169057\vjh~CgC\990561.@zz\15249#\1043141NKhbM\ENQ\1056272X]`\"d%>l\STX\ETB\RSX\fKa\NUL70IJ\992408jax6\DC1\1019378_Lm\DC3\STX!\1010649u\1027656S\1050838\1070417$g[i\FS,U8\986421\f8\DC3x}\1067081_YG\1109753\154135\DELGP\n.\tQ8u4\190011:\58201w\EOT#9}\1048123\&9^^H\1011479\43335\SYNC\ACK\ru\1089665\1017328\1083253:\1065162RQ#`T\SUBk=\996628=<\\\DEL)|\180725\175241K_-r<\rW\SOH\183066\1002046!\1034078\r|O4\EM\DC3\vdh\146722!\ETB-\1111680\b\ENQ\n^_\996690<\43822])\EM\54770p\169924\ETBk\1001553\&7\vEFP\a\1054724\SOHn|a^\998605\35304\ETBkuA\ENQC7\57818\NAK\f<\NAKk]]-8\DC1^\159338aX#\150097\53800O\NAK-\ETB$/\23533i\SOHhnq;o\70425\1104108\1064636\NAK\1057129-\vc\1013255\rd\RS\fBc\120033\1011836/N\51756A\SI=rW\2018\986318\1015098S\93830\EM\1102644l\1068589?\16266\1102049&Z\1090101,\152718<h1\1015783\1025183\n\a]\78366e\1030446\&5\191375h\fGl`\ETXIx\999212\SOf=K\179851e\1093147a\167556l\RS\1098161\DC3\992847p/\rT\"4k{$A\163659\FS\1100377\1031410?GL)\DC1+\DC2u\1071822NpaQ\ESC\1009603\987574\68669QU\DLE\bS\68765v\a!\992974:P;UPe\140038\"\173046\997929\1007369$DOIMx\3360\&2+\n\EOT\SYN7d\NAK\1019303h\1024573\&0\b5\24975\1101091A&r9C(t\1057084u\170538Ybi|/0\1066302\990243N\113675M'\1010905>\DEL\DLE\v\1033235\1015493\SIY\SUB\140063],(\n%\EM3gwe\183172\&2\DEL\892\998102\b\162593\1039784B\CAN\49073\RS\EOT\DC4\8095\DC2RW\1072719\94282Kn\DLE\ETBR\STX",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate ""))),
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) Nothing],
      newServiceTags = unsafeRange (fromList [VideoTag])
    }

testObject_NewService_provider_20 :: NewService
testObject_NewService_provider_20 =
  NewService
    { newServiceName =
        Name
          { fromName =
              "\t\35592$\NAK\ETBh\152542\SUB\SYN*[\999698\175733@\167039_\1095165_\DC14&o\120717a\SYN\SOH[Xn1=}\1041235'1\EMO\DEL\v.yD\r\EMC\STX\DC3R\SOt\136353i\994454B\EOTf\n\128016J y\40122%d^!z8s\1089111"
          },
      newServiceSummary =
        unsafeRange
          "\52590\165897\EM\1005681\&4\1045043\174219\147250f\129112\167748\aW,\62247\&4\1109928\138352\SUBR\CANe\62066TZs3rx&\SYN\995011p\1095215\t'74E&\SO@4Cy\ETX!bx\DC3AFZK\DC1\1108965T9NT\190754[\GS@\CAN&u\ETX8\"z!\29247N\154850\&6=\1085397\156509\SOH~\DC2\SOH",
      newServiceDescr =
        unsafeRange
          "\62751\141576.?}\1108888Za\64279n\STX3\57621\1062138~%Vk(\38880V$\STXxY\4213m\1078503a\51691\r\DELL\US\SUBG\165228sG*\1024201A\ESC\34112\a4\SYNC\DC4\CAN3\t5\150955\NUL\1043168\1057513\14591\&2\b\RSj!IU;7\182988\fk~87\146614j)\165776\ENQ8\f<\1043781\94759\177810\1097111\STX7I\144519i\1060002>M;g\EMd;\t\FS\1111955B\31284p\EM\ACKd@e\NAKj^\151993@X\EMRL\1023971\SUB\19537rI\DC1\165134\FSto)*Z\1067648Z?\f\ETXE\174943_\34037\SIa5!;|\986743\1110668`0\SOHG\t\CANp=\25139y\SYNA\1051288 \",\41442\19483\1015930\25649yj/^Z\t6!\1007169(,\1108180%P\1006295Xw-\996828\SOH\ETX\RSyr~7\1036625`&\a}y^\1025731I\b\1047931\&6e\GSG?\136467\FS\STXb;|\50085\66880\FS\1106975j\RS\1023826ag\"a42\1057605\19361#@|Hw\160257:h\ESC\n\3521\US\168733R#\20752%(/dd\EOT}A'l\1087376y\1106854VC\1093579}6\DLE\DC4(\1072423\rqS>\173519Jdrv\141146\1081015\ENQ\ACKS\ESCSg\1073100.E\SYN{KLv/p\183414UwUQ\v%\1053837n=\132343\180426s6\83409\DEL!?\1735\1067222x&}\FS#C[\ETX\18724\SOHa^\175569Q@\143616\1000854j\1008285\SOc,K\20391\1080149>F@\11914\&5]\SUB\990355\1112405\48753,;a\1005723\&0\1036530'\NAK\NULSH\ETX\DC1)\ETBJ\ACKo\NUL^\37410\1055553\165594x`\NAK\1102235\998909\DELY\ACK\GS\1016410h\16294wm\32491C{\EM\129420) ,D\EOT\DC4v7:f\1087942]2HM\181418{I\118905\&1KFTT6>\143142W\190042S]B!-\62255\SO^\1021758\165409D\RS\58898!=j\24112d>&1+s\EM^\14688i\1048206\EMH\RS\144705+\DC4DH\rPCmi\GS\CAN\141543l\1018968\&4\1051067lb\156521\&3\46238\187992?\US\51615\1014110\&3\1085511\f\a\8499M()\ETB\1107566\EMm\187060\24226\NUL\NULf\NULb?a2c\1084885*\NULxZn-/N+\ESCc\NULf\SUBi\1097319l\FSP\917987\155835&Y%$@O4\STX;\162206\EOTn\12039/IO\1058888f\DC4+A\SYN/\NAK[\FS\EOT\61354\&74\24181\EMMDfl\STX\SUB\983501\984985\CANd\15172p I\a_}\23094\1020442it\1037080H'Fa\DC2\1078367FQ)w\1108389#\NULj )\b\1084732^\SO/e\1085624\DLE>hb\167627\1011378V.\f\SOH\14358!\129498\100476\SOL\29746\DC2*\987207YCUmz\DLE\1010566\1016595\173119f9",
      newServiceUrl =
        coerce
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
            },
      newServiceKey =
        ServiceKeyPEM
          { unServiceKeyPEM =
              PEM
                { pemName = "PUBLIC KEY",
                  pemHeader = [],
                  pemContent =
                    "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                }
          },
      newServiceToken = Just (ServiceToken (fromRight undefined (validate "e4k="))),
      newServiceAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)],
      newServiceTags = unsafeRange (fromList [VideoTag])
    }
