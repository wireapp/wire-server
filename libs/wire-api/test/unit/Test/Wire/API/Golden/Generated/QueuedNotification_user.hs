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
testObject_QueuedNotification_user_1 = (queuedNotification ((Id (fromJust (UUID.fromString "00000067-0000-002c-0000-002800000065")))) ((List1 (NonEmpty.fromList [fromList [("S:\1046421K\45050\95215\142360 ",Bool False),("\ETX)j\1000825p\n31",Object (fromList [("z",Null),("",Number (-2.0e-2)),("\US",Number (100.0)),("#",String "")])),(",a\1015986#\97228\SOH",Null)],fromList [("(p\138250\39273fd\958{\ETXNh",Array []),("m\164493\100133\185344\1074452\RS\DC1\132081\11293\a\GS\DELX",Array [Bool False]),("_",Number (2.0e12)),("\ACKi\1047139&\SO4hel\140583\167612\&6",String "Y\US")]]))))
testObject_QueuedNotification_user_2 :: QueuedNotification
testObject_QueuedNotification_user_2 = (queuedNotification ((Id (fromJust (UUID.fromString "00000019-0000-0017-0000-000900000061")))) ((List1 (NonEmpty.fromList [fromList [("",String "\CAN(\SI+|\ACKG"),("\ENQH\1112848/\ESCM\SYN$0\33868\25083\92621foK",Number (-0.8)),("%\DC2V&q\1113167f~\68392\GS\1071053\EMx",Bool True),("99G-@r",Object (fromList [("\1010144FOU\1014479\29791\1035504\EOT",String "\",\1048615]/ \17162\96586\EMgX")]))],fromList [("8g\ESC",Array []),("\ETB\156320\"\30752,",Bool True),("\\_?\RS-",Object (fromList [("\996914~",Number (1.0e-5))])),("L\1089893\992521",Object (fromList [("",Null)])),("I",Object (fromList []))],fromList [("",Array [Bool True,Number (10.0),Number (10.0)]),("K)\989426x",Array [Number (0.0),Bool True,Null]),("X\GS5\SUB",Object (fromList [])),("\141863$\1085953",Bool False)],fromList [("\DC3@",Array [Null]),("N\FS\1027972",Object (fromList [])),("d\167711\1067045",Object (fromList [("\DC4\f~",String "*\1052732f")]))]]))))
testObject_QueuedNotification_user_3 :: QueuedNotification
testObject_QueuedNotification_user_3 = (queuedNotification ((Id (fromJust (UUID.fromString "0000003b-0000-001a-0000-004d0000005a")))) ((List1 (NonEmpty.fromList [fromList [("\ENQ]6\\\DC3<9",String ""),("-R",Object (fromList [("\178559",Number (20.0)),("",String "X\ETX"),("!\985165V",Number (-10.0))])),("",Bool False),("sh\DC4n\DC1^",Array [Number (-30.0),String "sE@",Number (-5.0e-5)]),("C",Array [Bool True,String "U",Null,Number (-1.0),Null,Bool True,String "",Bool False,Number (-10.0)])],fromList [("/\1087876\1011680",Null),("\150221",Array [Number (-100.0)])],fromList [("\1089668\&6Xz]",Array [Bool True,Bool False,Bool False,Number (0.0),Bool False,Bool False,Bool True]),("\1041987\&2\ACK\EOT\DC3",Bool False),(",N[L\v",Array [Number (0.0),Bool True,String "",Null,Number (0.0),Number (0.0)]),("s$\RS",Object (fromList [("\DC1\r",String "")]))],fromList [("\na4",Array []),("\61016",Array [Number (0.0),Number (0.0),Number (0.0),String "Z",Bool True]),("\ru",Array [Number (-2.0e-3)]),("xt|\t\6440",Array [Bool False,Bool True,String "\SYN",Null])]]))))
testObject_QueuedNotification_user_4 :: QueuedNotification
testObject_QueuedNotification_user_4 = (queuedNotification ((Id (fromJust (UUID.fromString "0000005a-0000-0024-0000-000d00000074")))) ((List1 (NonEmpty.fromList [fromList [("\SUB>\1076013J\DLE",String "5"),("\998682fPpi\1019665Y.",String "zD\DC4\194683\125251$ug\1064926\1041011")]]))))
testObject_QueuedNotification_user_5 :: QueuedNotification
testObject_QueuedNotification_user_5 = (queuedNotification ((Id (fromJust (UUID.fromString "00000071-0000-000d-0000-002100000040")))) ((List1 (NonEmpty.fromList [fromList [("\125083\DLEC\134542",Array []),("\1044693\&31\b\CAN\994405\1064171\SOHr\1015365",Array [Bool False,Number (0.0),Number (-2.0e-2)]),("[\186826%=\SI\176824#\t[\1017357\78748\1077967=F\1039634",Bool False),("Ai\1009491\SUBt_\50003\EM\32099\\\145151aC",Array []),("\1034142\DC3VH\SOii\129633\NULl?XE\EM0",Array [String "",Null,String ""]),("h*Q\NULb",Object (fromList [])),("\CAN:~F\1110545\RSeB_i",Array [Null]),("\DC4\1021010\54165\&4\3978J\154412j",Object (fromList [("",Number (0.0))])),("\brQF",Object (fromList [])),("\NAK",Object (fromList []))],fromList [("",Array [Bool False,Null])],fromList [("\n",Object (fromList [("y",String "\49985")]))],fromList [("",Array [Null])],fromList [],fromList [],fromList [],fromList [],fromList [("",Number (1.0))],fromList [("",Object (fromList []))],fromList []]))))
