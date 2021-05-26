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
module Test.Wire.API.Golden.Generated.UserUpdate_user where

import Imports (Maybe (Just, Nothing))
import Wire.API.User
  ( Asset (ImageAsset),
    AssetSize (AssetComplete, AssetPreview),
    ColourId (ColourId, fromColourId),
    Name (Name, fromName),
    Pict (Pict, fromPict),
    UserUpdate (..),
  )

testObject_UserUpdate_user_1 :: UserUpdate
testObject_UserUpdate_user_1 =
  UserUpdate
    { uupName = Nothing,
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Just [(ImageAsset "" (Nothing)), (ImageAsset "" (Nothing))],
      uupAccentId = Just (ColourId {fromColourId = 2})
    }

testObject_UserUpdate_user_2 :: UserUpdate
testObject_UserUpdate_user_2 =
  UserUpdate
    { uupName = Just (Name {fromName = "~\RSK\1033973w\EMd\156648\59199g"}),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Just [(ImageAsset "" (Just AssetComplete))],
      uupAccentId = Just (ColourId {fromColourId = 3})
    }

testObject_UserUpdate_user_3 :: UserUpdate
testObject_UserUpdate_user_3 =
  UserUpdate
    { uupName = Nothing,
      uupPict = Just (Pict {fromPict = []}),
      uupAssets =
        Just
          [ (ImageAsset "`\34002" (Nothing)),
            (ImageAsset "" (Just AssetComplete)),
            (ImageAsset "4\ACKE" (Just AssetPreview)),
            (ImageAsset "" (Nothing)),
            (ImageAsset "" (Nothing)),
            (ImageAsset "" (Nothing)),
            (ImageAsset "\61750\STXf" (Just AssetPreview))
          ],
      uupAccentId = Just (ColourId {fromColourId = -5})
    }

testObject_UserUpdate_user_4 :: UserUpdate
testObject_UserUpdate_user_4 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\18082\\5-`\f\155562T\SUB|\rD\147442GV\1078111\46527nt@,5\1007825Ux0M\1093197\CANrf\46052+\1003319\CAN\153789\&4L\32397]+\DELu\NUL\1023019\t6\164426z\fo\21287\NUL\151080d\NUL\SUB\96525\a\GS"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets =
        Just
          [ (ImageAsset "W%" (Just AssetPreview)),
            (ImageAsset "" (Nothing)),
            (ImageAsset "" (Just AssetPreview)),
            (ImageAsset "e\SYN\1013544" (Just AssetPreview))
          ],
      uupAccentId = Just (ColourId {fromColourId = 2})
    }

testObject_UserUpdate_user_5 :: UserUpdate
testObject_UserUpdate_user_5 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "}C)BJ\SUB\60353M1\FS\1052672\DLE\186121\fT\US}lG}B4D\FS/Y\1055890\DC4g\1033925I\992008\n%\172154p"
              }
          ),
      uupPict = Nothing,
      uupAssets = Just [(ImageAsset "R\1000805" (Just AssetComplete))],
      uupAccentId = Just (ColourId {fromColourId = -8})
    }

testObject_UserUpdate_user_6 :: UserUpdate
testObject_UserUpdate_user_6 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\179443\7145\&6U5!E\1026324nyH\1060078\94836B$\DLE\DEL\DC3-\DC1\998885\1040778\1028475rO\ESC\a\FS\1027937\1102166`i`\158910m/YzFX:ZRz\40972\FS\994216\161648\&75"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Just [],
      uupAccentId = Just (ColourId {fromColourId = -2})
    }

testObject_UserUpdate_user_7 :: UserUpdate
testObject_UserUpdate_user_7 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\f/\r\1072381\160399\NUL\1076257}A[D \173047\EM]Q\ACK#:!q\59840+$&\ETB\179839#\SI\b^2\v\46403\fsPB\1012154<?\178465C(\1048000\a\DEL\994013\1038842i%.\"y\DC1g\NULY\28208B&\181449nM\28044a\1054636W\1025500{N6Q:\27115d\frt&\148413\RSGj0\EOTAs\1003503\1013885\SOH\989613\1090333\n5}c\t\aJ\EM\rK"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Just [(ImageAsset "nC" (Just AssetPreview))],
      uupAccentId = Just (ColourId {fromColourId = 6})
    }

testObject_UserUpdate_user_8 :: UserUpdate
testObject_UserUpdate_user_8 =
  UserUpdate
    { uupName = Nothing,
      uupPict = Nothing,
      uupAssets = Just [(ImageAsset "EGD" (Just AssetComplete)), (ImageAsset "6\1107548\DC2" (Just AssetPreview))],
      uupAccentId = Just (ColourId {fromColourId = 8})
    }

testObject_UserUpdate_user_9 :: UserUpdate
testObject_UserUpdate_user_9 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\tj?\DC2\RSx{\bPL\134745\1062317Iy\NAKX\"\NAK\ETXg|\173729+Z\184935\&3:\v5n\28160\SOl\ACK(\31199\NUL\1022194\1081801\1040704#3\118936\999989B'F\178739*\SO\RSe0SE\ENQ!\157968W\SYN\1102948./wvth\DC3I\1113413~Js\NAKb'(\SYNzi\35704[\v"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets =
        Just
          [ (ImageAsset "" (Nothing)),
            (ImageAsset "\188439\DC3/" (Just AssetPreview)),
            (ImageAsset "k" (Just AssetComplete))
          ],
      uupAccentId = Just (ColourId {fromColourId = -5})
    }

testObject_UserUpdate_user_10 :: UserUpdate
testObject_UserUpdate_user_10 =
  UserUpdate
    { uupName = Nothing,
      uupPict = Just (Pict {fromPict = []}),
      uupAssets =
        Just
          [ (ImageAsset "\SOH" (Just AssetComplete)),
            (ImageAsset "\161805(\37333" (Nothing)),
            (ImageAsset "\15804\&9" (Just AssetPreview)),
            (ImageAsset "D" (Just AssetPreview)),
            (ImageAsset "F" (Just AssetComplete)),
            (ImageAsset "" (Just AssetPreview))
          ],
      uupAccentId = Just (ColourId {fromColourId = 2})
    }

testObject_UserUpdate_user_11 :: UserUpdate
testObject_UserUpdate_user_11 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\EM\SYN\62027C\160764\&75O\175069a>\1076290HB+e<Y\1007908f\1060060\NAK\60002\&6\1342\b\DC4Fk\1046132\&5m!BO\1027123o\r7\DLE\59226]O%\187914\984562\29210F\37282+O\5097%%\ESC\132163#i\1085991\142858\a\1049410\ESC>&%\r\US256\186183HHa\"or!uV\NAKT\64042<\EMpH"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets =
        Just [(ImageAsset "v" (Nothing)), (ImageAsset "" (Nothing)), (ImageAsset "\CAN\GS" (Just AssetPreview))],
      uupAccentId = Nothing
    }

testObject_UserUpdate_user_12 :: UserUpdate
testObject_UserUpdate_user_12 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\r\1039482Z\CAN&zv\1014151\ACKvz\1100570\158367\987894ex=3\STX\1092502\n;@t\5575t,!Dx\1026936~\"iks\b!R\177837MT\134815\FS%\CANg\20839\bx\GS\1105601\188616\&4V\SOH\1090559\RS\DLE\26049.\14980a\992707\EOT_+M\NUL8GY%\n\1094563\NUL\58828}!tIB"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Nothing,
      uupAccentId = Just (ColourId {fromColourId = 6})
    }

testObject_UserUpdate_user_13 :: UserUpdate
testObject_UserUpdate_user_13 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\1044671\&9k\bE\NUL\"9d\NAK\1109755\&8Z\15202.(+: _7As~\125226\SOH\140843\1032516D\1085357yC$\996510Q\DC1\DC4\1014012\&6[\1034518\175709?\4988P\34644\&1!N\n\1068831\&4V\FSk\NUL\SOH_-\37112\1021531\n;&m\ENQ\t\166024D1V25\61064^\137352#s\1066203\&2l\175875\SI\1061300\187602\GS\1063439[ L\178759V\1097869\DC2\US\19305\b\100486\1036805K\1014116:UMM1H\tk"
              }
          ),
      uupPict = Nothing,
      uupAssets =
        Just
          [ (ImageAsset "?w" (Just AssetComplete)),
            (ImageAsset "" (Just AssetComplete)),
            (ImageAsset "\1077824\&1" (Just AssetPreview)),
            (ImageAsset "i\ACK\63652" (Nothing)),
            (ImageAsset "\1087556\DC3\191297" (Nothing)),
            (ImageAsset "U\1103637," (Just AssetComplete))
          ],
      uupAccentId = Just (ColourId {fromColourId = 2})
    }

testObject_UserUpdate_user_14 :: UserUpdate
testObject_UserUpdate_user_14 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "/H\DC1Toc\DC4\DC1\54115`{0\154001[\SOH<\150233\1072626\NAKm\ETX|Wd|\1008161\DC3\GS\1039875\SOHn\1100849\140588wuN\1054281tvB/En|2^n\171256_\58506K\1007777\&4\CAN\ETB\1013763\1087777D\165659R'q6\DC4?\DC2\STXAEO((W\1004794r'C\1018863\SUB\STX+M\SOE\DC3hi\CAN\49550\&8\187020\NUL#\CAN\9913Ex\141641\134184Av&=&%\131212H}p\23993grJ\120368d"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Just [(ImageAsset "\64831\121090" (Just AssetComplete))],
      uupAccentId = Nothing
    }

testObject_UserUpdate_user_15 :: UserUpdate
testObject_UserUpdate_user_15 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\16814e\155596:\992888\NULz\DC2\SUBmx\180351\1037179\v\42268u\1073830J\54625#\4459]|\1112860%\111253~zM6*-\93029~\CANXm\180894R!v\1069937Nano\1086481uf7\1072061TP\ETB\152372\127219\191108!O\NAK\42281\ACK\ESC\DC4\65939\EOTH|\38224K\GS1,RzU`(\NUL\156865\177574:4\\%\179080dc\12331w\vd\n\1026663\152722\1098863\SO\1001375d\EOT\1088813G\EOT0\1058000\US\NAKtH\CANN\147871\48387E\RS\1007015"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets =
        Just
          [ (ImageAsset "\166996" (Just AssetPreview)),
            (ImageAsset "" (Just AssetPreview)),
            (ImageAsset "%" (Just AssetPreview))
          ],
      uupAccentId = Just (ColourId {fromColourId = -2})
    }

testObject_UserUpdate_user_16 :: UserUpdate
testObject_UserUpdate_user_16 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\ncV~sD!\1057793\34727sU\DLE\t>*\NUL,h<\1097292\DEL.l^X\US\187241oc9;\1077389\1056051\NAKnofI~\135565\DC2L\1094163\1096027\ESC\EOT\1031678;\\%\1005629$\167754\&6e\1090985?0\SIAk"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Just [],
      uupAccentId = Nothing
    }

testObject_UserUpdate_user_17 :: UserUpdate
testObject_UserUpdate_user_17 =
  UserUpdate
    { uupName = Nothing,
      uupPict = Just (Pict {fromPict = []}),
      uupAssets =
        Just
          [ (ImageAsset ")E\100683" (Nothing)),
            (ImageAsset "c\DC2/" (Just AssetPreview)),
            (ImageAsset "6OT" (Nothing)),
            (ImageAsset "B" (Just AssetComplete))
          ],
      uupAccentId = Just (ColourId {fromColourId = 6})
    }

testObject_UserUpdate_user_18 :: UserUpdate
testObject_UserUpdate_user_18 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "\DC4}\191258$.3Z\SYNX\1021301:\STX23\133378\1077097v\1087008\CAN\DC3\ETBr2%sf\a\\\1046301\ACK\30017{$,\120262\EMx\SO-t\45592\986316y\RS\180423R\SOHG\160762\"\CAN.\155473\162373`UJe\"\r\a=.2\NAK\GS\ETXW\SO\1083705n*2\1072082@V\DELK\SYN@Q,@E_~\ENQi\ESC~\1057217;!II,k07\1091310\&2\62319\ETX\ESC"
              }
          ),
      uupPict = Just (Pict {fromPict = []}),
      uupAssets =
        Just
          [ (ImageAsset "r\1011600s" (Nothing)),
            (ImageAsset "\DEL" (Nothing)),
            (ImageAsset "H" (Just AssetComplete)),
            (ImageAsset "" (Nothing))
          ],
      uupAccentId = Just (ColourId {fromColourId = 7})
    }

testObject_UserUpdate_user_19 :: UserUpdate
testObject_UserUpdate_user_19 =
  UserUpdate
    { uupName = Nothing,
      uupPict = Just (Pict {fromPict = []}),
      uupAssets = Nothing,
      uupAccentId = Just (ColourId {fromColourId = 0})
    }

testObject_UserUpdate_user_20 :: UserUpdate
testObject_UserUpdate_user_20 =
  UserUpdate
    { uupName =
        Just
          ( Name
              { fromName =
                  "D]980\984137)e\991819:?s$QP\31836_\DC4qyj\154724\138286\DC4x\SUBq6uwz\181356\36245\1049734\DC4\1068581\SO\SOH\SUBz\1046764D\r\ETX\1079395\&6NK\1019667f/OK\97244\ENQr\EOTy*\NAK\USA{\ETB}\NAKrr(X^`\t_W}L\1053312\fUy\SUB\r\138279sM\4542\GS7\f\1030887:\1063938B\GS\1064215\STX)\1001003k\ESCj\DC1\NAK\997417;f*\DEL"
              }
          ),
      uupPict = Nothing,
      uupAssets = Nothing,
      uupAccentId = Just (ColourId {fromColourId = -1})
    }
