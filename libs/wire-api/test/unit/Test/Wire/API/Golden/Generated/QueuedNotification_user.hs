{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.QueuedNotification_user where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_QueuedNotification_1 :: QueuedNotification
testObject_QueuedNotification_1 = (queuedNotification ((Id (fromJust (UUID.fromString "00000060-0000-0008-0000-003300000064")))) ((List1 (NonEmpty.fromList [fromList [("\60458uJ2YW\nnH\DC2\1076560Y~\DC1H",Null),("w6u\29225",Object (fromList [("U\1093229\1111924\1042088\SOH",Number (0.0)),("A6T*\nU\45323",Number (6000.0))])),("\992561C\EM",Array []),("k/W&m9\DC1}<|0\154289\DLE\1053049",Array []),("\b\1040790\33088\18306\1111108\1107006\ETX\15713N",String "\STX"),("j\148616K\190630",Object (fromList [("\1092654",Null),("\FS",Number (-1.0)),("",Bool True),("Ar",String ""),("qX",Number (0.2)),("\996309\&7",Null)])),("8Y<\1026652D\FS)\1034484\476;",Object (fromList [])),("\31093\SUB\n:k\FS\CAN\SISA",Null),("\42325\&0-)jp\142183\RSW",Null),(";\EOT; \1084909\&7$\ACK\1020895\1011187:\DC3",Object (fromList [("",String "\ENQ|"),("[",Null),("ZI",String "W\ESC")]))],fromList [("q&Z\t`",Object (fromList [("",String ""),("(^",Bool False)])),("\161948\1002501\SUB(\f\EM",Bool False),("\DLE`",Array [String "\22446\1071887"]),("1-\142646\120809\1007216\&98",Number (50.0)),("\DC4\179449Z",Bool True),("Ms\24789",Object (fromList [])),("\158641\1044447",Number (-1.0e-6))],fromList [("4pY",Array [Null,String ""]),("",Null),("%\989165",Object (fromList [])),("w9\1004642",Array [Bool False])]]))))
testObject_QueuedNotification_2 :: QueuedNotification
testObject_QueuedNotification_2 = (queuedNotification ((Id (fromJust (UUID.fromString "00000028-0000-000a-0000-005800000035")))) ((List1 (NonEmpty.fromList [fromList [],fromList [("",Object (fromList [("",String "\FS")]))],fromList [("\13799\1007254",String "\CAN\ETX")],fromList [],fromList [("?",Array [])],fromList [],fromList [],fromList []]))))
testObject_QueuedNotification_3 :: QueuedNotification
testObject_QueuedNotification_3 = (queuedNotification ((Id (fromJust (UUID.fromString "0000004b-0000-0012-0000-004f00000009")))) ((List1 (NonEmpty.fromList [fromList [(".",Array [Bool False]),("$",Array [String "",Null,Null,String "",Number (0.0),String "",Bool True,Bool True,Number (0.0),Null,Null,Null,Number (0.0),Number (0.0),String "",Number (0.0)]),("\DC1d\SOA'j\1076202@,\1059472W\1063993\180576\41149k",Object (fromList [])),("\33793*_\29053",Number (9.0e7))],fromList [],fromList [("\STX\\u",Object (fromList [])),("D",Bool True)],fromList [("i",Object (fromList []))],fromList [("=D\SO",Object (fromList [("",Bool False),("N",Number (1.0)),("E",Null)])),("\131147\ENQ",Number (3.0e-2)),("\SUB7",Number (-0.2))],fromList [("",Object (fromList []))]]))))
testObject_QueuedNotification_4 :: QueuedNotification
testObject_QueuedNotification_4 = (queuedNotification ((Id (fromJust (UUID.fromString "00000000-0000-0063-0000-002f00000036")))) ((List1 (NonEmpty.fromList [fromList [("iV\SYNA",Array [Bool False,Number (20.0),Null,String "\n",Bool False,Number (0.0),Bool True]),("P\147696F\128249\1111428\ENQ5\44628\21315\180790\t\171121m9v",String "qLrZ"),("\CAN\63870C\1046424\&7\EMR\142540y\36434g\b\984744Z",Array [Null,Number (0.0),String "",Number (0.0),Null,Bool True,Null,Number (0.1),Bool False])]]))))
testObject_QueuedNotification_5 :: QueuedNotification
testObject_QueuedNotification_5 = (queuedNotification ((Id (fromJust (UUID.fromString "00000015-0000-0027-0000-00030000000c")))) ((List1 (NonEmpty.fromList [fromList [("\GS",Null),("\SYN-\64957\1012061\ACK\SYN\1047845\179880?l\fwToY",Number (1.3e-14)),("\ETX%.",Bool True),("\149315XD-\983111\DC2>",Object (fromList [("\FS\ETBB\CAN\ETX\fNB5D",Bool False)])),("",Object (fromList [("",Null),("`",Bool False),("\US",Null),("x",Number (-0.1)),("\SYN",String "j")])),("l\1065996\37653\1113072Uw",Array []),("\1051816\CAN\ACK\n\1029629Ho",Array [Number (30000.0)]),("ob\NULZ\DC1X",String "\997442\179704\&2O\1095361=\FS\ETXt"),("\8574J\1039705.8+I\16369\156482\169039:\15063f\136766",Object (fromList [("\31921\EMS\t\SOH",Bool False),("\DC2\185648\132319\"\62303",Null),("\172562n\SOH",Null)])),("%x\DC2\DC2\EOTEp\988771 \ENQ\1110096\1054543zV",Object (fromList [("\1030084\DC4=",Bool False),("\1086964/L",Null),("d ",Null)])),("\68161O,\DC2\SUB,\191431\&9\RS7RY",Array []),("h1J-\1035242",Bool True),("p\989430ehV\1099492\1060576M 8\ACKxJrF",Object (fromList [("Gf\rd",String ",P\v\25009Dd"),("\DC1\1102563",Number (-4000000.0))]))],fromList [("\180896{[\1104229#\1090035\120507&\EM",Object (fromList [("\STX_m\a\43204~\160419\DC2U\1088766A",Bool False)])),("e\996338\&4\SOH\1054497",String "H"),("m\181539\39429\EMnDZ2e",Null),("@\32270",Null),("\37257~\78615u\162422!\ACK\36164q\1104117",Bool True),("h\1097251(`\1057493x.E\RSCr",Object (fromList [("",Null)])),("5",Bool False),("m'\NULhd3z\1049052",Bool True),("\US\b)\92416\ETX",Array []),("\2348\FS\v",Bool True)]]))))
