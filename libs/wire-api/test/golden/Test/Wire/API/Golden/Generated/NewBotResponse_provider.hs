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
module Test.Wire.API.Golden.Generated.NewBotResponse_provider where

import Imports (Maybe (Just, Nothing))
import Wire.API.Provider.External (NewBotResponse (..))
import Wire.API.User.Client.Prekey (Prekey (Prekey, prekeyId, prekeyKey), PrekeyId (PrekeyId, keyId), lastPrekey)
import Wire.API.User.Profile
  ( Asset (ImageAsset),
    AssetSize (AssetComplete, AssetPreview),
    ColourId (ColourId, fromColourId),
    Name (Name, fromName),
  )

testObject_NewBotResponse_provider_1 :: NewBotResponse
testObject_NewBotResponse_provider_1 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1079194"},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("+\1035266\ENQ")),
      rsNewBotName =
        Just
          ( Name
              { fromName =
                  "s\vw` \158953\"\1105754\1069511Z\174068\DC3`| \SOH8\169336c+F3G\119663\1102382\188004TC\14138(hrV\FSqe^v&\120111Cs\186596\1042800F}a>|.#A\1037988\78420\ETX\37424\1008162T"
              }
          ),
      rsNewBotColour = Just (ColourId {fromColourId = 0}),
      rsNewBotAssets = Nothing
    }

testObject_NewBotResponse_provider_2 :: NewBotResponse
testObject_NewBotResponse_provider_2 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("\158260S\1013700\1033003\997116")),
      rsNewBotName = Just (Name {fromName = "\185552}nqW\t\179361\&7f"}),
      rsNewBotColour = Nothing,
      rsNewBotAssets =
        Just
          [ (ImageAsset "C#\1056358" (Just AssetComplete)),
            (ImageAsset "\DC4\n" (Just AssetComplete)),
            (ImageAsset "V" (Just AssetComplete)),
            (ImageAsset "Y+_" (Just AssetComplete))
          ]
    }

testObject_NewBotResponse_provider_3 :: NewBotResponse
testObject_NewBotResponse_provider_3 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("I")),
      rsNewBotName =
        Just
          ( Name
              { fromName =
                  "\"\157432\&7\994903\29876\177822\NAKbXB_0\1108926\&5\51061gU\1034557/b:\42152\1105836Rr\1013128\983899\98378u\121008V\f\16860?f\\\EOTx\DLEx~\33789xi\1056990S2MSKK\SYNcpb[\a\1014311\&8\136231\FSx\48117U\120371X\ACK}\94970\995192\&6B\1081245,oZ\SYNl\SI\SYNf\27390\&8M26\165723\1002734\NULse,4\DEL`\STX2\186433Lk\ESCMc"
              }
          ),
      rsNewBotColour = Just (ColourId {fromColourId = 0}),
      rsNewBotAssets =
        Just
          [ (ImageAsset "'\DC2" (Nothing)),
            (ImageAsset "`" (Just AssetPreview)),
            (ImageAsset "?\1084357\ESC" (Just AssetPreview))
          ]
    }

testObject_NewBotResponse_provider_4 :: NewBotResponse
testObject_NewBotResponse_provider_4 =
  NewBotResponse
    { rsNewBotPrekeys = [],
      rsNewBotLastPrekey = (lastPrekey ("\DC4G)K\1059819\\")),
      rsNewBotName =
        Just (Name {fromName = "WmX!\1028903 B7\ACK\140127\1012306C\SUB\1037988F\1043143i\DLE\f$\a\1100404\ESC9\DLED"}),
      rsNewBotColour = Just (ColourId {fromColourId = 8}),
      rsNewBotAssets = Nothing
    }

testObject_NewBotResponse_provider_5 :: NewBotResponse
testObject_NewBotResponse_provider_5 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "U"},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("\fC\NULL\\\EOT")),
      rsNewBotName = Nothing,
      rsNewBotColour = Just (ColourId {fromColourId = 7}),
      rsNewBotAssets = Just []
    }

testObject_NewBotResponse_provider_6 :: NewBotResponse
testObject_NewBotResponse_provider_6 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\29859"}
        ],
      rsNewBotLastPrekey = (lastPrekey ("")),
      rsNewBotName =
        Just
          ( Name
              { fromName =
                  "qo\148126\ACK6x\DC3\EOT]\b\1094989hdg]\46488$\189177\1087680\ETB\NUL\"b\131895\150626\126097\SOHgf(w\1004565\99881<\USu\50045\DLE#\22110\ACK\1078678\DC3\STX\1086290xvyCN\1035435\74199\1019237\DC4\996305\&6Z,I>\195079\ACK\DEL.\SO>\20175~S\GS\1004416\t\96771\1089396\1043764%%8\7085\ENQX\SIpLk\v\1090653K"
              }
          ),
      rsNewBotColour = Nothing,
      rsNewBotAssets = Nothing
    }

testObject_NewBotResponse_provider_7 :: NewBotResponse
testObject_NewBotResponse_provider_7 =
  NewBotResponse
    { rsNewBotPrekeys = [],
      rsNewBotLastPrekey = (lastPrekey ("")),
      rsNewBotName =
        Just
          ( Name
              { fromName =
                  "hce\37636\145655\&8\59131 grK\92399\ESC)\141855EQj{\STX\132497\DC4!^\35494}\168069\GS\993947\DC2z\992842\171464\"\SUB <\SOH{ov(\DC3\135589V@H\DC4\131593\182715\&1\1107112`Y\92488_\128664vaxL\aT\ESC%m{\EOT29A\166554\188028\100865b\1067423H0\183435J\DC4\1105460W\1017216_\ENQ5[%Zc'X<w#\tD\EOT\7710\&2\168632JP"
              }
          ),
      rsNewBotColour = Nothing,
      rsNewBotAssets = Nothing
    }

testObject_NewBotResponse_provider_8 :: NewBotResponse
testObject_NewBotResponse_provider_8 =
  NewBotResponse
    { rsNewBotPrekeys = [],
      rsNewBotLastPrekey = (lastPrekey ("F!i\14754\987148")),
      rsNewBotName =
        Just
          ( Name
              { fromName =
                  "o#o\1000426l\ACK\187579k\aow\42066\GSi >\1047371*Am\18981\NAKd\995000\&1\DC2o\nRo/\1017928L\US:h\1000458\992844M\r28r\n3\140293\ETX\v\1093154\1055908\&3\1031760\FSe\r\1061028U\ETB\181448\1007774b\51444\EOTyMtg\NAKF\b\1026593\&2\164240#&\1520\ENQr\1025119Dx"
              }
          ),
      rsNewBotColour = Nothing,
      rsNewBotAssets =
        Just
          [ (ImageAsset "\5691~" (Just AssetPreview)),
            (ImageAsset "i\97515" (Just AssetPreview)),
            (ImageAsset "\\" (Nothing)),
            (ImageAsset "%o" (Just AssetPreview))
          ]
    }

testObject_NewBotResponse_provider_9 :: NewBotResponse
testObject_NewBotResponse_provider_9 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\NAK"},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("]4\68421\&8\\")),
      rsNewBotName =
        Just
          ( Name
              { fromName =
                  "\"H\1051779\29336\1046195a\1104721n\EMrC\ENQH\50152.=\1034431D?(\1000253Ve\182813\1068525Z\n@\1053745X*A\1050380\\\UST\993554[~$Je"
              }
          ),
      rsNewBotColour = Just (ColourId {fromColourId = -5}),
      rsNewBotAssets = Just []
    }

testObject_NewBotResponse_provider_10 :: NewBotResponse
testObject_NewBotResponse_provider_10 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("R\165465")),
      rsNewBotName = Nothing,
      rsNewBotColour = Just (ColourId {fromColourId = -5}),
      rsNewBotAssets = Just [(ImageAsset "" (Nothing))]
    }

testObject_NewBotResponse_provider_11 :: NewBotResponse
testObject_NewBotResponse_provider_11 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "8"}
        ],
      rsNewBotLastPrekey = (lastPrekey ("U\STX\ETB\1112642x")),
      rsNewBotName = Nothing,
      rsNewBotColour = Nothing,
      rsNewBotAssets = Nothing
    }

testObject_NewBotResponse_provider_12 :: NewBotResponse
testObject_NewBotResponse_provider_12 =
  NewBotResponse
    { rsNewBotPrekeys = [],
      rsNewBotLastPrekey = (lastPrekey ("\EM\FS")),
      rsNewBotName = Nothing,
      rsNewBotColour = Nothing,
      rsNewBotAssets = Nothing
    }

testObject_NewBotResponse_provider_13 :: NewBotResponse
testObject_NewBotResponse_provider_13 =
  NewBotResponse
    { rsNewBotPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "h\131368I"}],
      rsNewBotLastPrekey = (lastPrekey ("\1000435eP'X")),
      rsNewBotName =
        Just
          ( Name
              { fromName = "Pcf\DLEj\ACK\1097398\a\SUB_V}*p\989699!O\r\DC4u\13775\1055377J\NAKl\f\EOTE\1007156&\"D:ly|"
              }
          ),
      rsNewBotColour = Just (ColourId {fromColourId = -6}),
      rsNewBotAssets =
        Just
          [ (ImageAsset "\DLEk" (Just AssetPreview)),
            (ImageAsset "/" (Nothing)),
            (ImageAsset "" (Just AssetPreview)),
            (ImageAsset "" (Nothing)),
            (ImageAsset "\21612\188425" (Just AssetComplete)),
            (ImageAsset "\f\1077258E" (Just AssetPreview))
          ]
    }

testObject_NewBotResponse_provider_14 :: NewBotResponse
testObject_NewBotResponse_provider_14 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("Q\1012726\39031U")),
      rsNewBotName =
        Just
          ( Name
              { fromName =
                  "pVuv\1103746AD;\ACK\149305=\997932fie5R\1019930\15886 \175045S\133044\13450\EOTE[\DLE\151030\&4ZN+Vj\DLE6\DC2\135812D\1075037jda\132974\&8H)\1053603k\51644`(\SOp@\58329H\f0M\17816\1058095J}\DC2|\1102034\1049531\59661O\DC2\NUL\ETB5o\v"
              }
          ),
      rsNewBotColour = Just (ColourId {fromColourId = -4}),
      rsNewBotAssets = Nothing
    }

testObject_NewBotResponse_provider_15 :: NewBotResponse
testObject_NewBotResponse_provider_15 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("KuA\172666\1084633")),
      rsNewBotName =
        Just
          ( Name
              { fromName =
                  "B0J4V\ENQ#1\148632\98095\1076824?;\a\"\ESCd:>b\tJ\157077\ACK\23000F7\10988q\1058117\DC1k:U\1054386\69847\159048:h\164355G_\DLE},<\984170\167325zQ\NAKW?-x;Iq?U\68642\EOT/x\179272\a"
              }
          ),
      rsNewBotColour = Nothing,
      rsNewBotAssets = Nothing
    }

testObject_NewBotResponse_provider_16 :: NewBotResponse
testObject_NewBotResponse_provider_16 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("e!D*j")),
      rsNewBotName = Just (Name {fromName = "\174414\&4?rvqg%\DC2\167142\DC1t\CAN\62298\SI_\92287F"}),
      rsNewBotColour = Just (ColourId {fromColourId = -5}),
      rsNewBotAssets = Just [(ImageAsset "\"\63981\1047766" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))]
    }

testObject_NewBotResponse_provider_17 :: NewBotResponse
testObject_NewBotResponse_provider_17 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "b"}
        ],
      rsNewBotLastPrekey = (lastPrekey ("\1064414\f\1024452\12105")),
      rsNewBotName = Just (Name {fromName = "g\49675B{\DC3Cq\CANmbD\DEL5Q\DC4>i\DC4\SI[\1022068|K\44297\57731|\175014"}),
      rsNewBotColour = Just (ColourId {fromColourId = 1}),
      rsNewBotAssets = Just []
    }

testObject_NewBotResponse_provider_18 :: NewBotResponse
testObject_NewBotResponse_provider_18 =
  NewBotResponse
    { rsNewBotPrekeys = [],
      rsNewBotLastPrekey = (lastPrekey ("\21089N|.\GS")),
      rsNewBotName = Nothing,
      rsNewBotColour = Just (ColourId {fromColourId = 8}),
      rsNewBotAssets =
        Just
          [ (ImageAsset "\FS" (Just AssetComplete)),
            (ImageAsset "\92915\984145" (Just AssetPreview)),
            (ImageAsset "" (Just AssetPreview))
          ]
    }

testObject_NewBotResponse_provider_19 :: NewBotResponse
testObject_NewBotResponse_provider_19 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("u=\NAK")),
      rsNewBotName = Just (Name {fromName = "FvrT0g\\\169897"}),
      rsNewBotColour = Nothing,
      rsNewBotAssets =
        Just
          [ (ImageAsset "\158941\DC1" (Just AssetComplete)),
            (ImageAsset "\t" (Just AssetPreview)),
            (ImageAsset "V#" (Just AssetComplete)),
            (ImageAsset "" (Just AssetPreview)),
            (ImageAsset "(\ETX" (Just AssetComplete))
          ]
    }

testObject_NewBotResponse_provider_20 :: NewBotResponse
testObject_NewBotResponse_provider_20 =
  NewBotResponse
    { rsNewBotPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "+"},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\52025"},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      rsNewBotLastPrekey = (lastPrekey ("`|\144284^\US")),
      rsNewBotName = Nothing,
      rsNewBotColour = Nothing,
      rsNewBotAssets =
        Just
          [ (ImageAsset "\"" (Just AssetPreview)),
            (ImageAsset "\1076571d" (Just AssetPreview)),
            (ImageAsset "8" (Just AssetComplete))
          ]
    }
