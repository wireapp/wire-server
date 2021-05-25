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
module Test.Wire.API.Golden.Generated.PushTokenList_user where

import Data.Id (ClientId (ClientId, client))
import Wire.API.Push.Token
  ( AppName (AppName, appNameText),
    PushTokenList (..),
    Token (Token, tokenText),
    Transport (APNS, APNSSandbox, APNSVoIP, APNSVoIPSandbox, GCM),
    pushToken,
  )

testObject_PushTokenList_user_1 :: PushTokenList
testObject_PushTokenList_user_1 =
  PushTokenList
    { pushTokens =
        [ (pushToken (GCM) (AppName {appNameText = "p\DELU2r"}) (Token {tokenText = "MK8p\f"}) (ClientId {client = "4"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "\n\163637`\1098480>\191239"})
              (ClientId {client = "1a"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\EM\ETX"})
              (Token {tokenText = "\181637"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\29574Y\1096493O"})
              (Token {tokenText = ""})
              (ClientId {client = "13"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = ">8C9E"})
              (Token {tokenText = "\STX\1025439\ESCW\1024542\&2\SOH"})
              (ClientId {client = "18"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\fn\ENQE\RS"})
              (Token {tokenText = "\1051986\38460\12861\1083961\1074746\""})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "H>8\ETX"})
              (Token {tokenText = ")\b v"})
              (ClientId {client = "12"})
          ),
          (pushToken (GCM) (AppName {appNameText = "V"}) (Token {tokenText = ""}) (ClientId {client = "16"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\1093930\ETB"})
              (Token {tokenText = "\EM3"})
              (ClientId {client = "1a"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "`B\136938\\"})
              (Token {tokenText = ">"})
              (ClientId {client = "1c"})
          )
        ]
    }

testObject_PushTokenList_user_2 :: PushTokenList
testObject_PushTokenList_user_2 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "\ETBa\1063308\&8\1039183"})
              (ClientId {client = "c"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\DC3"})
              (Token {tokenText = "wO+"})
              (ClientId {client = "8"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\ESC"})
              (Token {tokenText = "\1000123\61357)X\DC4"})
              (ClientId {client = "1b"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\a(\162592"})
              (Token {tokenText = "\1089554\170436ZgJE"})
              (ClientId {client = "1f"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "mY"})
              (Token {tokenText = "_lB#\989093"})
              (ClientId {client = "1a"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "#\175613+"})
              (Token {tokenText = "7#k\n\NULe"})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\14628\\Jb\157564"})
              (Token {tokenText = "\128910\SIY;aw\RS"})
              (ClientId {client = "10"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "c\1023262"})
              (Token {tokenText = "\tfS\1046266\183654*\1028550"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "5\CAN#a\t\1021535"})
              (Token {tokenText = "~"})
              (ClientId {client = "d"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = " \ETX:"})
              (Token {tokenText = "\SUBDS\143740"})
              (ClientId {client = "6"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "z\SO>\17205l\160502\1074572"})
              (Token {tokenText = "p7"})
              (ClientId {client = "b"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\EOTj\992654"})
              (Token {tokenText = "(}]\t\12197?"})
              (ClientId {client = "4"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\UST5\ETBG"})
              (Token {tokenText = "\37775+["})
              (ClientId {client = "3"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "4\1013283\1039903\EOT\STX."})
              (Token {tokenText = "\STX\32639\US"})
              (ClientId {client = "3"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "]C"})
              (Token {tokenText = "N\ESC\32609*_"})
              (ClientId {client = "4"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\13557B\48674v\SI)"})
              (Token {tokenText = ";\EOT\ESC\CAN"})
              (ClientId {client = "9"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\a<!<"})
              (Token {tokenText = "y\1081792"})
              (ClientId {client = "20"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "~\994098"})
              (Token {tokenText = "\46545a\1053177{\14089A"})
              (ClientId {client = "19"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\SO8.\1026065\&3C8"})
              (Token {tokenText = "ZIW"})
              (ClientId {client = "e"})
          )
        ]
    }

testObject_PushTokenList_user_3 :: PushTokenList
testObject_PushTokenList_user_3 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNS)
              (AppName {appNameText = "/\159372"})
              (Token {tokenText = ",|\ETX"})
              (ClientId {client = "0"})
          ),
          (pushToken (GCM) (AppName {appNameText = "\DLE"}) (Token {tokenText = "[\\iS"}) (ClientId {client = "6"})),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\GS\1111132\EOTS\177484"})
              (Token {tokenText = "NEU\62911oxG"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "]\7470\1080979}"})
              (Token {tokenText = "\159673R"})
              (ClientId {client = "10"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\1017921l\132177\1076421\RS\19463"})
              (Token {tokenText = "\SOH!\1064998\141541\RS"})
              (ClientId {client = "14"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "E\"7\135812\US"})
              (ClientId {client = "1"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\1056137K!"})
              (Token {tokenText = "'"})
              (ClientId {client = "1a"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "c\1065178\DEL\16137\9236\RS\1082164"})
              (Token {tokenText = "\NUL\1018030"})
              (ClientId {client = "1"})
          )
        ]
    }

testObject_PushTokenList_user_4 :: PushTokenList
testObject_PushTokenList_user_4 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "2R&"})
              (Token {tokenText = "G\r,i,Gk"})
              (ClientId {client = "11"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "g\SO$S\ETB5\n"})
              (ClientId {client = "2"})
          ),
          (pushToken (APNS) (AppName {appNameText = "_O"}) (Token {tokenText = "'\4536mt?,"}) (ClientId {client = "1a"})),
          (pushToken (GCM) (AppName {appNameText = "P}~\SO:"}) (Token {tokenText = "4eP=nj"}) (ClientId {client = "e"})),
          (pushToken (APNS) (AppName {appNameText = ""}) (Token {tokenText = ""}) (ClientId {client = "1e"})),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "B0S\1086619"})
              (Token {tokenText = "GwoD\1110658\EOT\74379"})
              (ClientId {client = "c"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\DC4\989426"})
              (Token {tokenText = "+$\f"})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "]\152463F\995841"})
              (ClientId {client = "18"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\r\985108D_"})
              (Token {tokenText = "."})
              (ClientId {client = "8"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = "ov"}) (Token {tokenText = "q"}) (ClientId {client = "19"})),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "L"})
              (Token {tokenText = "\US\SI4\b"})
              (ClientId {client = "d"})
          ),
          (pushToken (GCM) (AppName {appNameText = "\194622a"}) (Token {tokenText = "w6x"}) (ClientId {client = "c"})),
          ( pushToken
              (APNS)
              (AppName {appNameText = "NsGq"})
              (Token {tokenText = "{\EOT6to\45449"})
              (ClientId {client = "9"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "q\CANI\STXz0"})
              (Token {tokenText = "Pi"})
              (ClientId {client = "1c"})
          )
        ]
    }

testObject_PushTokenList_user_5 :: PushTokenList
testObject_PushTokenList_user_5 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNS)
              (AppName {appNameText = "\1046364\1011859\1051970\at\143987f"})
              (Token {tokenText = "=\\\1042600p]"})
              (ClientId {client = "d"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "=\US6\1025571\DC1Wn"})
              (Token {tokenText = "\n)\60902$\GS"})
              (ClientId {client = "a"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\n:a\145407\ESC\1061647\&7"})
              (Token {tokenText = "i\v[^opY"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\49894"})
              (Token {tokenText = "\189509\1017881\f\1015433\143015sd"})
              (ClientId {client = "12"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\45494x\DELIM"})
              (Token {tokenText = "\SYNL\1039144e\ENQ"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "v\SI"})
              (Token {tokenText = "]\1006320f"})
              (ClientId {client = "17"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\DEL6\STX"})
              (Token {tokenText = "\\"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\1002028?"})
              (Token {tokenText = "F\SOH\1108263\ENQ\1101725\&3"})
              (ClientId {client = "c"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\1113784\1102572|\118844\EM&"})
              (Token {tokenText = "\993828G\1031326\1067982"})
              (ClientId {client = "9"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "G!-(gD"})
              (Token {tokenText = "\CANE_\ETX\r"})
              (ClientId {client = "1b"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "S\167020U\1014164\1084918["})
              (Token {tokenText = ""})
              (ClientId {client = "14"})
          ),
          (pushToken (GCM) (AppName {appNameText = "\10971\28666k"}) (Token {tokenText = ""}) (ClientId {client = "7"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = ""})
              (Token {tokenText = "F3\166273&KZY"})
              (ClientId {client = "17"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\157776\1070392\999794\164112"})
              (Token {tokenText = "!3\1072604)\SOH"})
              (ClientId {client = "1c"})
          ),
          (pushToken (GCM) (AppName {appNameText = "#\183530"}) (Token {tokenText = ""}) (ClientId {client = "e"}))
        ]
    }

testObject_PushTokenList_user_6 :: PushTokenList
testObject_PushTokenList_user_6 =
  PushTokenList
    { pushTokens =
        [ (pushToken (APNS) (AppName {appNameText = ""}) (Token {tokenText = "#"}) (ClientId {client = "1"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "NJ\13824"})
              (Token {tokenText = "m"})
              (ClientId {client = "19"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\DC1"})
              (Token {tokenText = ""})
              (ClientId {client = "3"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\36018Ipi\177160\18119"})
              (Token {tokenText = ""})
              (ClientId {client = "2"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "'[\133852\100668c"})
              (Token {tokenText = "\1082078`\FSx\\X"})
              (ClientId {client = "1c"})
          ),
          (pushToken (APNS) (AppName {appNameText = ">"}) (Token {tokenText = "~Z\EOTt8"}) (ClientId {client = "6"})),
          ( pushToken
              (APNS)
              (AppName {appNameText = "f5e"})
              (Token {tokenText = "T\83099E\1094942\DC4\DC1\142242"})
              (ClientId {client = "6"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\b+3I\64154\1097165"})
              (Token {tokenText = ""})
              (ClientId {client = "20"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = ""})
              (Token {tokenText = "\132205\ESC4\1008274\1093375\74556"})
              (ClientId {client = "1f"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "f\1072151\&3^y"})
              (Token {tokenText = "b"})
              (ClientId {client = "d"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\188479>^\r"})
              (Token {tokenText = "uJf"})
              (ClientId {client = "11"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\133498Q\aY"})
              (Token {tokenText = "\a\34395P\171559\1016454"})
              (ClientId {client = "c"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\184508\1084974\68836;1N\DC3"})
              (Token {tokenText = "\SOf\DC3\ENQ"})
              (ClientId {client = "1e"})
          ),
          (pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = ""}) (ClientId {client = "20"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "[\US\GS~`"})
              (Token {tokenText = " d\1025577\36290"})
              (ClientId {client = "d"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\1028415"})
              (Token {tokenText = "m0\t5"})
              (ClientId {client = "18"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "CO3\DC3\1010630\NUL\1089070"})
              (Token {tokenText = "]"})
              (ClientId {client = "5"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "4"})
              (Token {tokenText = "\23563\CAN\66375M)\39081O"})
              (ClientId {client = "2"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\FS3\DC2\ETB\16475t)"})
              (Token {tokenText = "\167394\1000094h\EMm\ACK"})
              (ClientId {client = "20"})
          ),
          (pushToken (APNSSandbox) (AppName {appNameText = "5"}) (Token {tokenText = ""}) (ClientId {client = "1"})),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\1036710c\b4\54554X"})
              (Token {tokenText = "\\\DC2e8"})
              (ClientId {client = "10"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\ay=\SUB8\SUB\58780"})
              (Token {tokenText = "\ETBb7~}"})
              (ClientId {client = "7"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\51121\1105577\20857\CANU\120963m"})
              (Token {tokenText = "{"})
              (ClientId {client = "f"})
          )
        ]
    }

testObject_PushTokenList_user_7 :: PushTokenList
testObject_PushTokenList_user_7 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "oB\SO"})
              (Token {tokenText = "\15103q\1056626\1034486<\DC1\1049028"})
              (ClientId {client = "6"})
          ),
          (pushToken (APNS) (AppName {appNameText = "6\SI."}) (Token {tokenText = ""}) (ClientId {client = "1d"})),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\DC2Y\986105n\136674\""})
              (Token {tokenText = "\118955:e\188969"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "5s\USB"})
              (Token {tokenText = ")\SUB\SO\EM.X"})
              (ClientId {client = "7"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "r \nr>"})
              (Token {tokenText = "sd\1063106"})
              (ClientId {client = "9"})
          )
        ]
    }

testObject_PushTokenList_user_8 :: PushTokenList
testObject_PushTokenList_user_8 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNS)
              (AppName {appNameText = "\1093247\&4 \134867"})
              (Token {tokenText = "]-"})
              (ClientId {client = "18"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\983685\1022852"})
              (Token {tokenText = "H5"})
              (ClientId {client = "3"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\1081576"})
              (Token {tokenText = "\1048552El"})
              (ClientId {client = "10"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "b&\994956\ESC\1004059\&2"})
              (Token {tokenText = "\DC4O%\SUBb\EOTT"})
              (ClientId {client = "12"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\178739\t#$G "})
              (Token {tokenText = "\1014512o."})
              (ClientId {client = "18"})
          ),
          (pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "A"}) (ClientId {client = "18"})),
          (pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = ""}) (ClientId {client = "d"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "8\43536.e\DC3K"})
              (Token {tokenText = "\SYN"})
              (ClientId {client = "1"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\RSA"})
              (Token {tokenText = "8\DC4\SOHK"})
              (ClientId {client = "12"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\DLE/\r%\1078015D@"})
              (Token {tokenText = "Q\24447"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\EOT\1071073\1089139"})
              (Token {tokenText = "\b"})
              (ClientId {client = "5"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "q"})
              (Token {tokenText = "\184898\USS\189126"})
              (ClientId {client = "a"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "EN\21622\181405"})
              (Token {tokenText = "l\164632 m\DC1ob"})
              (ClientId {client = "1a"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\992637)\1065394"})
              (Token {tokenText = "\110813\CAN39"})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "9\1050943\1064467\53723w\RS"})
              (Token {tokenText = "\vefi\GS\42199\138040"})
              (ClientId {client = "18"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\186219/YS"})
              (Token {tokenText = "\1055639"})
              (ClientId {client = "10"})
          ),
          (pushToken (APNS) (AppName {appNameText = "="}) (Token {tokenText = "iv97\53846"}) (ClientId {client = "4"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "O\195038a"})
              (Token {tokenText = "EG4pJI\t"})
              (ClientId {client = "1a"})
          )
        ]
    }

testObject_PushTokenList_user_9 :: PushTokenList
testObject_PushTokenList_user_9 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "r\SI\b\rqN\a"})
              (ClientId {client = "b"})
          ),
          (pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = ""}) (ClientId {client = "14"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\1096025W["})
              (Token {tokenText = "o\DLE\26505"})
              (ClientId {client = "5"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = "n"}) (Token {tokenText = ""}) (ClientId {client = "4"})),
          (pushToken (APNS) (AppName {appNameText = "\145387pQ"}) (Token {tokenText = "<V"}) (ClientId {client = "1c"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\156163\ENQ\\\1063814/Kg"})
              (Token {tokenText = "\ENQ,\aB"})
              (ClientId {client = "8"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\1032122"})
              (Token {tokenText = "\\O\ETB@"})
              (ClientId {client = "1c"})
          ),
          (pushToken (APNS) (AppName {appNameText = "\3548%\SOvy"}) (Token {tokenText = ""}) (ClientId {client = "15"}))
        ]
    }

testObject_PushTokenList_user_10 :: PushTokenList
testObject_PushTokenList_user_10 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNS)
              (AppName {appNameText = "\DC3\ACK\162375\1101221\DELO\a"})
              (Token {tokenText = "\1018536\SO\161727"})
              (ClientId {client = "18"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\8090\1000348%"})
              (Token {tokenText = "s\ENQVun\1005035"})
              (ClientId {client = "7"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "I^\DEL1"})
              (Token {tokenText = "V|\170473\\"})
              (ClientId {client = "13"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\61895P\147746\38021\f"})
              (Token {tokenText = "!w\45228\1001710"})
              (ClientId {client = "9"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "|8V"})
              (Token {tokenText = "\b\194812y\NUL_\EM"})
              (ClientId {client = "f"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\ETX\RSC"})
              (Token {tokenText = "D\1041307E"})
              (ClientId {client = "17"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "t\126508\EOT8"})
              (Token {tokenText = "`\3402W\NAKf"})
              (ClientId {client = "b"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "r\16030\993384\51833\1048459"})
              (ClientId {client = "8"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "?\30044~i:\SOH"})
              (Token {tokenText = "\v(-}#U"})
              (ClientId {client = "10"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "]"})
              (Token {tokenText = "a@"})
              (ClientId {client = "0"})
          ),
          (pushToken (GCM) (AppName {appNameText = "B\RS"}) (Token {tokenText = ""}) (ClientId {client = "1d"})),
          (pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = ""}) (ClientId {client = "14"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "M\140208#\v#"})
              (Token {tokenText = "vp%2\ETX\ESC"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "\133489o\DC3aK"})
              (ClientId {client = "1c"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = "\SYNzezn"}) (Token {tokenText = "l"}) (ClientId {client = "1"})),
          ( pushToken
              (GCM)
              (AppName {appNameText = "y\1084633\EM\bSF\1103768"})
              (Token {tokenText = "\40469n<\78784"})
              (ClientId {client = "2"})
          ),
          (pushToken (GCM) (AppName {appNameText = "T\ENQ"}) (Token {tokenText = "z"}) (ClientId {client = "8"})),
          ( pushToken
              (APNS)
              (AppName {appNameText = "S\1110068E"})
              (Token {tokenText = "\1062660\1009981\NAK\32290"})
              (ClientId {client = "12"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\ETXq\3375\1092244\95123"})
              (Token {tokenText = "y\1048012cE\DC1"})
              (ClientId {client = "3"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "U7"})
              (Token {tokenText = "`\134613o"})
              (ClientId {client = "1c"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = "D-"}) (Token {tokenText = ""}) (ClientId {client = "0"}))
        ]
    }

testObject_PushTokenList_user_11 :: PushTokenList
testObject_PushTokenList_user_11 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\FSO#uoH"})
              (Token {tokenText = "\16610RP~\b"})
              (ClientId {client = "1e"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "b\1057247\bZ&\SYN"})
              (Token {tokenText = "\135818\53866!\DC1\ENQ"})
              (ClientId {client = "10"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = ")Di\SUB"}) (Token {tokenText = ""}) (ClientId {client = "6"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "5[#*\1075192m"})
              (Token {tokenText = "\1026003~"})
              (ClientId {client = "6"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = ""})
              (Token {tokenText = "\DLE\98068\NAKV/"})
              (ClientId {client = "15"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "4"})
              (Token {tokenText = "\ENQX\1093607MF_"})
              (ClientId {client = "f"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "b\1031685:\66362R\1041293\1077360"})
              (Token {tokenText = "M"})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "U"})
              (Token {tokenText = "\nBaP9\FS"})
              (ClientId {client = "20"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\9336"})
              (Token {tokenText = "\1080317\65711f\1004099"})
              (ClientId {client = "6"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "Y\DC2"})
              (Token {tokenText = "X\n1wv"})
              (ClientId {client = "1f"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\60408\158201Z"})
              (Token {tokenText = "\tx"})
              (ClientId {client = "1d"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "#!G\1109681."})
              (Token {tokenText = "vZ "})
              (ClientId {client = "7"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "M\42368$\SUBV"})
              (Token {tokenText = "T\1047491\DC1x="})
              (ClientId {client = "11"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\133572e<4P"})
              (Token {tokenText = "G"})
              (ClientId {client = "1d"})
          )
        ]
    }

testObject_PushTokenList_user_12 :: PushTokenList
testObject_PushTokenList_user_12 =
  PushTokenList
    { pushTokens =
        [ (pushToken (GCM) (AppName {appNameText = "`O8&\16125"}) (Token {tokenText = ">#"}) (ClientId {client = "1e"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "t2;b\73775[\ETB"})
              (Token {tokenText = "7u\167444"})
              (ClientId {client = "2"})
          ),
          (pushToken (APNS) (AppName {appNameText = ""}) (Token {tokenText = ":\vY"}) (ClientId {client = "15"})),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\DC4\n\1080841_"})
              (Token {tokenText = "\38914,u\1043649"})
              (ClientId {client = "4"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\r"})
              (Token {tokenText = "\170656\&6"})
              (ClientId {client = "a"})
          ),
          (pushToken (APNSSandbox) (AppName {appNameText = "7bVc"}) (Token {tokenText = "YW"}) (ClientId {client = "f"})),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\SYNOl\DLE\1082224\b"})
              (Token {tokenText = "\a0@"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "P\ETBt"})
              (Token {tokenText = "\1063980\DC2\1067158\51560y3"})
              (ClientId {client = "8"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = " \987853P"})
              (Token {tokenText = "X\1019079\1029687,"})
              (ClientId {client = "2"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\b\186166\2975%Y\DELe"})
              (Token {tokenText = "\GSs\23362\CAN\1048543;"})
              (ClientId {client = "11"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "q3"})
              (Token {tokenText = "\96576"})
              (ClientId {client = "5"})
          ),
          (pushToken (APNS) (AppName {appNameText = "b"}) (Token {tokenText = "\DC4"}) (ClientId {client = "15"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\38059|\1082189A\61431\74394"})
              (Token {tokenText = "T?["})
              (ClientId {client = "4"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\179985"})
              (Token {tokenText = "\984961\36065"})
              (ClientId {client = "12"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "%\31677"})
              (Token {tokenText = "#"})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\1004113%"})
              (Token {tokenText = "\149348\1044202\SOH6F7"})
              (ClientId {client = "6"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "!NS\168146"})
              (Token {tokenText = ",\1049258\1064307\154352D\38789|"})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\999775dJ"})
              (Token {tokenText = "|\v-\SUB]"})
              (ClientId {client = "6"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\SOHVX\74234"})
              (Token {tokenText = "@\a\77925.\r\DC3\EOT"})
              (ClientId {client = "3"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\t\154165gA"})
              (Token {tokenText = "\1017697\&7]\100427"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\ENQSk"})
              (Token {tokenText = "\168039\CAN\ENQ-\161213"})
              (ClientId {client = "9"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\SO\SOH\EOTo\EOT"})
              (Token {tokenText = "2\NULG_{"})
              (ClientId {client = "11"})
          ),
          (pushToken (GCM) (AppName {appNameText = "\65604\bX"}) (Token {tokenText = "\DC1"}) (ClientId {client = "3"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "c\ETXb\1110298"})
              (Token {tokenText = "c\SO"})
              (ClientId {client = "3"})
          ),
          (pushToken (APNS) (AppName {appNameText = "Ej\a\SOH"}) (Token {tokenText = ""}) (ClientId {client = "14"}))
        ]
    }

testObject_PushTokenList_user_13 :: PushTokenList
testObject_PushTokenList_user_13 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "!J^l"})
              (Token {tokenText = "\1035871\DC1]"})
              (ClientId {client = "b"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\ETBf"})
              (Token {tokenText = "\1078084\1055795f\188949\DC2Pc"})
              (ClientId {client = "15"})
          )
        ]
    }

testObject_PushTokenList_user_14 :: PushTokenList
testObject_PushTokenList_user_14 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "as\ETXW\1057218"})
              (Token {tokenText = "\GS"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\1094404\"x"})
              (Token {tokenText = ""})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "Y_\DEL\EM\49004\&8"})
              (Token {tokenText = "\USVq\NAK\SUB\10430"})
              (ClientId {client = "16"})
          ),
          (pushToken (APNSVoIPSandbox) (AppName {appNameText = ""}) (Token {tokenText = "d"}) (ClientId {client = "7"})),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "nq6\1015524\&0U\1062389"})
              (Token {tokenText = "6^5P\45687\1051701F"})
              (ClientId {client = "18"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "?;r"})
              (Token {tokenText = "\1096892\ETX"})
              (ClientId {client = "6"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "\1106578\&9"})
              (ClientId {client = "3"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\DC4r\1107266\b;"})
              (Token {tokenText = "R\1053272\178891"})
              (ClientId {client = "9"})
          ),
          (pushToken (GCM) (AppName {appNameText = "bIv"}) (Token {tokenText = "\STX"}) (ClientId {client = "c"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ":\62340(#\b"})
              (Token {tokenText = ""})
              (ClientId {client = "b"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "{\NUL\aZb!~"})
              (Token {tokenText = "B\97571Q}\58604"})
              (ClientId {client = "19"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\127832\186505Z\1003749#"})
              (Token {tokenText = "\30444YL4[O\DLE"})
              (ClientId {client = "f"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "B\SI?"})
              (Token {tokenText = "ROqs\1000507_\173582"})
              (ClientId {client = "10"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "E\DC2\CAN\93765"})
              (Token {tokenText = "Ejl;Zgc"})
              (ClientId {client = "19"})
          ),
          (pushToken (GCM) (AppName {appNameText = "\CANG"}) (Token {tokenText = "1<"}) (ClientId {client = "14"})),
          (pushToken (APNS) (AppName {appNameText = "!\57637Y"}) (Token {tokenText = "w!"}) (ClientId {client = "13"}))
        ]
    }

testObject_PushTokenList_user_15 :: PushTokenList
testObject_PushTokenList_user_15 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\181237\f\131302/l!\1110857"})
              (Token {tokenText = "\1057340R\1084908\tV\7833,"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\ETX\1045203\1001387:}"})
              (Token {tokenText = "f$PT*|\SO"})
              (ClientId {client = "8"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = "x%4k"}) (ClientId {client = "14"})),
          ( pushToken
              (APNS)
              (AppName {appNameText = "J5\127327i>r"})
              (Token {tokenText = "\FS\t"})
              (ClientId {client = "1b"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "[\SYN\1002162v"})
              (Token {tokenText = "\1015507\12310r@\ETXB\DC2"})
              (ClientId {client = "14"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = " c=D\NULg2"})
              (Token {tokenText = "_\RSg"})
              (ClientId {client = "17"})
          ),
          (pushToken (APNSSandbox) (AppName {appNameText = "q"}) (Token {tokenText = ""}) (ClientId {client = "1d"})),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "="})
              (Token {tokenText = "C!("})
              (ClientId {client = "e"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = "\54091"}) (Token {tokenText = "'"}) (ClientId {client = "6"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "d\CAN:`j;*"})
              (Token {tokenText = "EN\1025768\1018919"})
              (ClientId {client = "d"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\139704W_\97907"})
              (Token {tokenText = ""})
              (ClientId {client = "15"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "vr/+["})
              (Token {tokenText = ":^\1066182\&7\ETB["})
              (ClientId {client = "a"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "Z\1099909\1020730\NUL\1087668l"})
              (Token {tokenText = "\DLEH"})
              (ClientId {client = "17"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "V\180754$\EOTVJ"})
              (Token {tokenText = "5\136826F\1112361"})
              (ClientId {client = "20"})
          ),
          (pushToken (APNS) (AppName {appNameText = "\163961"}) (Token {tokenText = "'S"}) (ClientId {client = "3"})),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\1009392\183815\DC1y@"})
              (Token {tokenText = "w"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\SOHS\"\100262?s"})
              (Token {tokenText = "\136522"})
              (ClientId {client = "e"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "/\58785\SOHF\1007832o\26020"})
              (Token {tokenText = "f\1034951<Gz"})
              (ClientId {client = "4"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "'\66578"})
              (Token {tokenText = "\60495l\23343\1057245x\NUL"})
              (ClientId {client = "11"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = ",\1105750H*\1010267\146059\1087441"})
              (Token {tokenText = "L\NAK\64434"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "0\133103\t+\1102153"})
              (ClientId {client = "4"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "oO\100447"})
              (ClientId {client = "2"})
          )
        ]
    }

testObject_PushTokenList_user_16 :: PushTokenList
testObject_PushTokenList_user_16 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNS)
              (AppName {appNameText = "T\167856\39439\1015520"})
              (Token {tokenText = ""})
              (ClientId {client = "c"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = "r|"}) (Token {tokenText = "/"}) (ClientId {client = "b"})),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = ""})
              (Token {tokenText = "\44488i\63814\&2\NAK"})
              (ClientId {client = "a"})
          ),
          (pushToken (APNS) (AppName {appNameText = "\RS|d#w\SUB"}) (Token {tokenText = ""}) (ClientId {client = "f"})),
          (pushToken (GCM) (AppName {appNameText = "y\SI\f{"}) (Token {tokenText = "\RS"}) (ClientId {client = "e"})),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\SO\1015286J"})
              (Token {tokenText = "="})
              (ClientId {client = "20"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "x"})
              (Token {tokenText = "\17111\b\170016\RS\98226Bz"})
              (ClientId {client = "0"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\SI"})
              (Token {tokenText = ""})
              (ClientId {client = "13"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\ESC\33474ug\32900\ETB\\"})
              (Token {tokenText = "FoT7\n\141967\1089002"})
              (ClientId {client = "d"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "8q\25538k\EM\185153#"})
              (Token {tokenText = "#-NY\188826\ACK"})
              (ClientId {client = "13"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\133975EwvK!z"})
              (Token {tokenText = "\1068274!N*\USi"})
              (ClientId {client = "a"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\ESC\43864"})
              (Token {tokenText = "\1088460+"})
              (ClientId {client = "19"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "0\SOHn\NAK"})
              (Token {tokenText = "\138019#<\1030926"})
              (ClientId {client = "e"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "-\54284\1035869\FS\SO"})
              (Token {tokenText = ",fkyy\1067328"})
              (ClientId {client = "1d"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\SOH\\"})
              (Token {tokenText = "\SOHkr\38903\1020799"})
              (ClientId {client = "2"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\1109645\SUBr"})
              (Token {tokenText = "rt"})
              (ClientId {client = "1d"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\140416\95336"})
              (Token {tokenText = "\"\EOT6"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "9^y5"})
              (Token {tokenText = "\1073548]\187885c\1066081M"})
              (ClientId {client = "2"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "s\DEL"})
              (Token {tokenText = "Y:\DEL\ENQ\DC3\SO"})
              (ClientId {client = "1a"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "6\170636\169220\1026798N\23939A"})
              (Token {tokenText = "7"})
              (ClientId {client = "1d"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\1101750{B;\SI"})
              (Token {tokenText = "\1094362\184644\174325O?;"})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "\189040~r\1085566i"})
              (Token {tokenText = "q\58879<;%"})
              (ClientId {client = "7"})
          )
        ]
    }

testObject_PushTokenList_user_17 :: PushTokenList
testObject_PushTokenList_user_17 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "0\t"})
              (Token {tokenText = "q\EOT\1060549G\a\95664f"})
              (ClientId {client = "10"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\984076\179100\1000727h\\:"})
              (Token {tokenText = "\SUBZ\997871["})
              (ClientId {client = "0"})
          )
        ]
    }

testObject_PushTokenList_user_18 :: PushTokenList
testObject_PushTokenList_user_18 =
  PushTokenList
    { pushTokens =
        [ (pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "\876\&7"}) (ClientId {client = "11"})),
          ( pushToken
              (GCM)
              (AppName {appNameText = "x"})
              (Token {tokenText = "\1022767\DLE\DLEq\"g"})
              (ClientId {client = "8"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\NAK\1041649|0\1102527\STX"})
              (Token {tokenText = "q<\DLE"})
              (ClientId {client = "18"})
          ),
          (pushToken (GCM) (AppName {appNameText = "7;\162029"}) (Token {tokenText = ""}) (ClientId {client = "18"})),
          ( pushToken
              (APNSSandbox)
              (AppName {appNameText = "?\142609\DEL*\1056538"})
              (Token {tokenText = "<D\DC4h"})
              (ClientId {client = "c"})
          ),
          (pushToken (GCM) (AppName {appNameText = "\r;"}) (Token {tokenText = "H\SUB#3"}) (ClientId {client = "8"})),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\22970\98791\173509/g"})
              (Token {tokenText = "C,"})
              (ClientId {client = "20"})
          ),
          (pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = ""}) (ClientId {client = "1b"})),
          (pushToken (GCM) (AppName {appNameText = "\ACK"}) (Token {tokenText = "4"}) (ClientId {client = "1"})),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\SYN:r="})
              (Token {tokenText = "n\EMf\39247U"})
              (ClientId {client = "b"})
          ),
          (pushToken (APNS) (AppName {appNameText = ""}) (Token {tokenText = "r\CAN"}) (ClientId {client = "f"}))
        ]
    }

testObject_PushTokenList_user_19 :: PushTokenList
testObject_PushTokenList_user_19 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (GCM)
              (AppName {appNameText = "2\1032788"})
              (Token {tokenText = "\1064921+\1009649"})
              (ClientId {client = "1a"})
          ),
          (pushToken (APNS) (AppName {appNameText = "w"}) (Token {tokenText = "U"}) (ClientId {client = "19"})),
          (pushToken (APNSVoIP) (AppName {appNameText = "\DC1"}) (Token {tokenText = "\f\a"}) (ClientId {client = "c"})),
          (pushToken (GCM) (AppName {appNameText = "\SO\NAK"}) (Token {tokenText = ""}) (ClientId {client = "8"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "j\n\111027>_\RS}"})
              (Token {tokenText = "\78323%"})
              (ClientId {client = "16"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\1025206$\15121\EMQw"})
              (Token {tokenText = "\1000219\1102732z\1109638YY"})
              (ClientId {client = "15"})
          ),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "tu\31405@\1032787"})
              (Token {tokenText = "\EOT\151479nUJY"})
              (ClientId {client = "1c"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\1045884"})
              (Token {tokenText = "d\135913\DEL\SUBL"})
              (ClientId {client = "11"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\ENQ\158055\&4"})
              (Token {tokenText = "]\DC2"})
              (ClientId {client = "6"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = "\48633\SYN\983526\&1\9164@"})
              (Token {tokenText = "\1105779xf("})
              (ClientId {client = "17"})
          ),
          (pushToken (APNS) (AppName {appNameText = "\RS"}) (Token {tokenText = "i!l)E\SOg"}) (ClientId {client = "5"})),
          ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "+~?lH\1063472\&4"})
              (Token {tokenText = ""})
              (ClientId {client = "1"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\1086043\176914L\26531\1042840"})
              (Token {tokenText = "`\1069575"})
              (ClientId {client = "b"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "j`8?\ESC^"})
              (Token {tokenText = "_\1075349Bm"})
              (ClientId {client = "5"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "t \USE["})
              (Token {tokenText = "\SOH\162814\DEL<"})
              (ClientId {client = "1e"})
          )
        ]
    }

testObject_PushTokenList_user_20 :: PushTokenList
testObject_PushTokenList_user_20 =
  PushTokenList
    { pushTokens =
        [ ( pushToken
              (APNSVoIP)
              (AppName {appNameText = "\1014551"})
              (Token {tokenText = "\51624\DC2$\DC2w"})
              (ClientId {client = "e"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "50"})
              (Token {tokenText = "M"})
              (ClientId {client = "c"})
          ),
          ( pushToken
              (GCM)
              (AppName {appNameText = ""})
              (Token {tokenText = "\ESCl\1078085<Z\ACK"})
              (ClientId {client = "e"})
          ),
          ( pushToken
              (APNSVoIPSandbox)
              (AppName {appNameText = "\1077617,\DC2X\SO\1025859"})
              (Token {tokenText = "*\t"})
              (ClientId {client = "1d"})
          ),
          ( pushToken
              (APNS)
              (AppName {appNameText = "\10134$"})
              (Token {tokenText = "\\\183333&\SIq"})
              (ClientId {client = "12"})
          )
        ]
    }
