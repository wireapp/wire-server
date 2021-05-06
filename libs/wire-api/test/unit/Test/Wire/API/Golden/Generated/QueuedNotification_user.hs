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
testObject_QueuedNotification_user_1 :: QueuedNotification
testObject_QueuedNotification_user_1 = (queuedNotification ((Id (fromJust (UUID.fromString "00000027-0000-0066-0000-00690000000c")))) ((List1 (NonEmpty.fromList [fromList [("^oGJ`N\EM\GS:\190418\1103247e\1088141o\26245",Object (fromList [])),(":",Object (fromList [("",Number (1.0e-3)),("\183076T\984889",String "P\SUB\a"),("N\DLEX",String "")])),("\989285'\EOT\NUL\69680\&7&Zaa\1047397^M{",Array [Bool False,String "\EOTPe",Bool False,Number (0.2)]),("5\113799\\*\984213\148370i\b\1020050",String "\DEL1\156457\&6\1055468\1047103\r"),("\36332",Object (fromList [])),("\1104730z",Array [String ""])],fromList [("",Array [String "",String "\DLERa"])],fromList [("\54703T",Array [Bool True]),("%\177209c\1039445mC",Array [Bool True,String ""]),("\SI",Bool False),("x\36300\GS\994743\23774_7",Bool False),("Y",Number (-4000.0))]]))))
testObject_QueuedNotification_user_2 :: QueuedNotification
testObject_QueuedNotification_user_2 = (queuedNotification ((Id (fromJust (UUID.fromString "00000055-0000-004b-0000-007e00000035")))) ((List1 (NonEmpty.fromList [fromList [("",Object (fromList [("A\983283\1025025J\1025210\&2\181371`\1040055]m\bI",String "\FS")])),("\1034339\&5\180719#\133625\25752\DC4\ETX8\178134d\DC4",Object (fromList [])),("h?gW\SI\DLE;^\DEL\t-",Object (fromList [("\DC3\DC3#.\1062161",Bool False),("\NUL`LEV",String "z5Rd"),("U",Number (-500.0))])),("\STXF\127969Jq*\RSPE",String ""),("]D\DEL\153905\1010822#s\NAKf",Array [Null,Null,String ""]),("9}\SOTYP\ETX\1044683\ETB<i",Object (fromList [("L@K\1048651",Null),("X$5",Number (0.0))])),("%l\1069243\FS",Object (fromList [(":",String ""),("`\100359Dw\ETX",Bool True)])),("\RS\NAK6\SYN\1011218",Array [Bool True,Number (-10.0),Number (0.0),String "",Bool True,Null,Null,Number (-1.0),Bool True,Null]),("cx\ra\989112\ACK7D\1015835\US\1016081\1068578\1060016",Number (-1.4e11)),("\SUB2!\1110200",String "a,\68322P\23587\&3"),("!\169077aE\DLEtSp\r\EM\GS\SOi1j",Object (fromList [])),("\GS:\SOH\ETX\f\160181\DC3Y0v\EM3",Array [Null])],fromList [],fromList [("\1010683N9\127081",Array [String "",Number (0.0),Bool False,Bool False,Bool True,Bool True]),("",Bool False),(" JK",Object (fromList [])),("RP8\ESC",Object (fromList [("G",Bool False),("",Null),("X",Null)])),("\r\DC3\ESCn;",Number (5.0))],fromList [("\1032010vYf",Object (fromList [("\DELW",Number (-3.0e-4))])),("a<\SUB\FSo",Object (fromList [("",Bool False)])),("\\\139545\986997\172696Y",Array [Number (0.0),Bool True,Number (0.0),Null,Number (0.0),Bool False,String ""]),("AW\NULY\r",Object (fromList [("",String ""),("1",String ""),("\40029",Null)])),("\b\1108341rH",Bool True)]]))))
testObject_QueuedNotification_user_3 :: QueuedNotification
testObject_QueuedNotification_user_3 = (queuedNotification ((Id (fromJust (UUID.fromString "0000006d-0000-0039-0000-00680000005a")))) ((List1 (NonEmpty.fromList [fromList [("\1016928",Array [String "M",Number (-1.0),Number (1.0),Number (0.0),Bool False,Number (-0.1),Null,Null]),("to\156874",Number (0.0)),("3<",Number (4.0e-2)),("\ACK\165883\ESC",Null),("\138540q\1041991",Array [String "",Number (-2.0e-3),Number (-2.0e-5)]),("",Object (fromList [("'\SO5",Bool False),("\NAK",Number (3.0e-3)),("AL",Null),("\1106306L",Number (-3000.0)),("!\1056367=",Bool True)])),("\t>3v\1103450\DC2s\1090315\190969{\53831\ETX\FS",Array [String "",Null,Bool False,Bool False,Number (0.0),Number (1.0),Bool False,Number (-1.0),Bool True,Bool True]),("\SO>\1047990\33929\NULKn\ETBK",Bool False),("#){\28431\1106574{\1013651k",Object (fromList [("\SUB\997281E\SOH^S\SI\141665",Number (2.0e7))])),("C",Object (fromList [(" \173415S\ENQifa\fA0@wU",Null)])),("Th1\997403\1057202\148821k0:\52495}\42618\GS\ETBs",Object (fromList [("",Number (6.0e11))]))],fromList [("",Null)],fromList [("\"\164696\a",Object (fromList [("",Null)]))],fromList [("",String ";*"),(" ",Array [Bool False,Number (-10.0)])],fromList [("N",Object (fromList [("R",String "")])),("B",String "rc\SO")]]))))
testObject_QueuedNotification_user_4 :: QueuedNotification
testObject_QueuedNotification_user_4 = (queuedNotification ((Id (fromJust (UUID.fromString "00000021-0000-0057-0000-000e00000016")))) ((List1 (NonEmpty.fromList [fromList [("> \ETB\29169Qdv@\FS",Number (5.0e-10)),("Z]34$^U",Array [Bool False,Bool True,Null]),("k\ETX\171083\42129z\32719k]~",Object (fromList [("",String "\1063359\NULl"),("\NAKt\185497",Number (3.0)),("ZH!\1040010",Null)])),(".{,\44578>\187395=\1044344\SO\141622\1104730",Null),("\1042402}<q~D\64306",Object (fromList [("\1065751",Number (-10.0)),("m",Bool False),("",Bool True),(" ",Bool True),("$",Number (-0.1)),("\1032065",Number (10.0)),("\32304",Bool False),("H",Null)])),("\ETBiVfp{\157109\DC2\4138\RS",Object (fromList [])),("\SO\1060713q!\1047417\GSF\30685\183666\37904\DC1\1002678*J",Array []),("\r\1033437k\165289\1087878\40813\1050314!>\24587",String "\174722iz6"),("0\RSK",Object (fromList [("",Null),("\US",Null),("?",String "z"),("3",String "")])),("\"=\DC3:8]\996402\1010490",Array [Number (-3000.0),Null,Bool True,Null,Number (0.3)]),("z\ETB\1024754D8D\61182Mz9U",Object (fromList [("",Null),("`1",Null),("\vXV",String "")])),("l\159407f\1008131\\Z%q\180084\171620\&1\EM\US\DC3?",Object (fromList [("\1012605 R6\140979",Bool False),("",String "\96291#\STXW\53889`")])),("Wx~\"\DLEB",Array [Null,Null,Null,Null,Number (0.0),Null,Number (0.0),Number (0.0),Number (0.0),Number (0.0),Null,Bool True,Null,Null,Null,Bool False,String "",Number (0.0),String "",Null,Null,Bool True,Number (0.0),Number (0.0),String "",Null,Null,Number (0.0),Bool False,Number (0.0),Bool True,Number (0.0),Number (0.0),String "",Number (0.0),String "",Number (0.0),String "",String ""])],fromList [],fromList [("\FS",String "")],fromList [],fromList [],fromList [("%",Array [])],fromList [],fromList [],fromList [("6",Object (fromList [("",Null)]))],fromList [("L",Object (fromList []))],fromList [],fromList [("\v",Array [Number (-1.0)])],fromList [],fromList [],fromList []]))))
testObject_QueuedNotification_user_5 :: QueuedNotification
testObject_QueuedNotification_user_5 = (queuedNotification ((Id (fromJust (UUID.fromString "00000018-0000-0064-0000-001e00000029")))) ((List1 (NonEmpty.fromList [fromList [("\35182\DLEg,\GS\1021155\NAK\178369.3\SI\1025829",Number (-5.0e9)),("w\SI~\STX",Object (fromList [("\1031881",Null),("}",Number (0.0)),("\1049617",Null),("",String "\SUB"),("`",String "\1026101"),("a",Bool False),("e",Bool False),("\43760",Bool True),("I",Null)])),(".\1075366\1055935\1110075",Null),("Z\DELkT\31235S\32800\&4S\USF\t",Array [Null]),("g\1046713\STX)\164981;W{[",Null),("\SYN\1022166q",String "q"),("!\DLE\ETB\f\DC3\DC3\SUB\ETXK<\1018309",Array []),("\1090466\15060\RS45\\\1062005\&9T`\10514`.D",Object (fromList [])),("Rd\989512\a\1111730\NUL\1055294\47644W\15498\&6@'",String "\1052168B=O\t\186394\FS6\1013619\DEL\DLE"),("G\145556\&7x\62577>\NAK_\SYN\169932[",Object (fromList [])),("E\36179\1005262\100434\EM+I",Bool False),("f\1099141\1057872=y.",String "Bw\1011600\GSN_y\DEL"),("\61512O\ESC\CAN \1013481",Object (fromList [])),("\SUB\t@Jn\1006543",Array []),(" D\178358\SOHCT\DEL[\SO|oYg",Null)],fromList [(",]\GS",Array []),("3)",Object (fromList [("",String "")])),("%",Object (fromList [("",String "\EOT'"),("N\18199",Null)])),("\ETBPD",String "")],fromList [("s",Array [Number (-2.0),String ""])],fromList []]))))
