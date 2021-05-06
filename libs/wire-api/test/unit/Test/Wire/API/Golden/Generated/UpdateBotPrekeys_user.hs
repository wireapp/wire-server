{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateBotPrekeys_user where

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
testObject_UpdateBotPrekeys_user_1 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_1 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "rz\145158\50637\47263 h\v6\")TT"},Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "\ETXQ'\62512H"},Prekey {prekeyId = PrekeyId {keyId = 32}, prekeyKey = "\"o"},Prekey {prekeyId = PrekeyId {keyId = 69}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 73}, prekeyKey = "U\139044\1087891\137816\n\DC3\a?"},Prekey {prekeyId = PrekeyId {keyId = 108}, prekeyKey = "s\NAKj&\1051103c,It\FS\1042087z3\n"},Prekey {prekeyId = PrekeyId {keyId = 24}, prekeyKey = "$k\22892\SIK"},Prekey {prekeyId = PrekeyId {keyId = 91}, prekeyKey = "\t\DLEa\1010925\DC2\74220\30197\150127m\\"},Prekey {prekeyId = PrekeyId {keyId = 16}, prekeyKey = "\\Ol\177388"},Prekey {prekeyId = PrekeyId {keyId = 75}, prekeyKey = "+\EOT\fr(\SUBR\69990\1112570yr\NUL~\US\30061"}]}
testObject_UpdateBotPrekeys_user_2 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_2 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 116}, prekeyKey = "\""},Prekey {prekeyId = PrekeyId {keyId = 33}, prekeyKey = "\ETXwCx\17717\49250'\177643"},Prekey {prekeyId = PrekeyId {keyId = 110}, prekeyKey = "&[_7\FS&\1070808"}]}
testObject_UpdateBotPrekeys_user_3 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_3 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 35}, prekeyKey = "{\f\DEL\1080227 Cj~\DLE3\166415B"},Prekey {prekeyId = PrekeyId {keyId = 5}, prekeyKey = "kMaG\63787\38718BL<<\98208\100679\158969"},Prekey {prekeyId = PrekeyId {keyId = 115}, prekeyKey = "\23041' T\59629\184837jI\35293E-"},Prekey {prekeyId = PrekeyId {keyId = 63}, prekeyKey = "y^"},Prekey {prekeyId = PrekeyId {keyId = 91}, prekeyKey = "\1080540Td\1014830I\22666\1016314\NULAS"},Prekey {prekeyId = PrekeyId {keyId = 63}, prekeyKey = "c\STXI\n\179201"},Prekey {prekeyId = PrekeyId {keyId = 84}, prekeyKey = "\179118\ENQT\SUB\FS\1001238u\DLE?5\150738\991864[v\1038411"},Prekey {prekeyId = PrekeyId {keyId = 42}, prekeyKey = "\146261?\156054"},Prekey {prekeyId = PrekeyId {keyId = 24}, prekeyKey = "\t"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "5"},Prekey {prekeyId = PrekeyId {keyId = 79}, prekeyKey = "l;\156720\1006030"},Prekey {prekeyId = PrekeyId {keyId = 73}, prekeyKey = "RT"},Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = "\\k\37224\1008928I"},Prekey {prekeyId = PrekeyId {keyId = 68}, prekeyKey = "f/\35625"},Prekey {prekeyId = PrekeyId {keyId = 105}, prekeyKey = "o\1045822\986214\DEL/Gl$"},Prekey {prekeyId = PrekeyId {keyId = 23}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 12}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 111}, prekeyKey = "%g|i\SYN\148079~"},Prekey {prekeyId = PrekeyId {keyId = 79}, prekeyKey = "\"\1002406&"},Prekey {prekeyId = PrekeyId {keyId = 57}, prekeyKey = "|<*fkuu\ETB|"},Prekey {prekeyId = PrekeyId {keyId = 68}, prekeyKey = "tFg\EM\158508\167152w\1058625\b\v\1049462"}]}
testObject_UpdateBotPrekeys_user_4 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_4 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "\23300`\1085630L\60543i"},Prekey {prekeyId = PrekeyId {keyId = 36}, prekeyKey = "\t\DC22\1043318\SYNR\175484~s\"QH"},Prekey {prekeyId = PrekeyId {keyId = 92}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 99}, prekeyKey = "^EAU\RS\a\1109327H\ETX=\n?\1061762"},Prekey {prekeyId = PrekeyId {keyId = 114}, prekeyKey = "\RS"},Prekey {prekeyId = PrekeyId {keyId = 43}, prekeyKey = "nDF\STX4d\1035321\EOTcdS3D}\r"},Prekey {prekeyId = PrekeyId {keyId = 126}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 9}, prekeyKey = "\164731\996626ueNV"},Prekey {prekeyId = PrekeyId {keyId = 99}, prekeyKey = "\DC4\1005144)%d\1045870\&5\1020292\v$K"},Prekey {prekeyId = PrekeyId {keyId = 19}, prekeyKey = "h NB~7@\131793\USW\EMIq"},Prekey {prekeyId = PrekeyId {keyId = 39}, prekeyKey = ""}]}
testObject_UpdateBotPrekeys_user_5 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_5 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 94}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 54}, prekeyKey = "e]R^i\1019797\174060\ETB\ETB0\1103712rI\RS\v"},Prekey {prekeyId = PrekeyId {keyId = 80}, prekeyKey = "\CAN\171347@;$M"},Prekey {prekeyId = PrekeyId {keyId = 45}, prekeyKey = "\bEP\126111?\DC2\1040483\139671\\"},Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\1086869H\ETB\29098U"},Prekey {prekeyId = PrekeyId {keyId = 89}, prekeyKey = "9\1035046Su`%\1096437\1054735\ACKvx\GS"},Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = "\SI\61521 mY?]\13555I\136589OBe"},Prekey {prekeyId = PrekeyId {keyId = 17}, prekeyKey = "v\1055992Y\171750'Z1\USP\1105428B"},Prekey {prekeyId = PrekeyId {keyId = 45}, prekeyKey = "\ACK7\"\99235\33466R"},Prekey {prekeyId = PrekeyId {keyId = 19}, prekeyKey = "\689wviJ|K\1069583?"},Prekey {prekeyId = PrekeyId {keyId = 104}, prekeyKey = "\1030807\163478\36395\45364\&3\SYN"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "t8;O"},Prekey {prekeyId = PrekeyId {keyId = 12}, prekeyKey = "\EOT\25670\SO\4477q\180669K\NAK\131914xR4\ETX\173663"},Prekey {prekeyId = PrekeyId {keyId = 123}, prekeyKey = "\US\37581N\1111966"},Prekey {prekeyId = PrekeyId {keyId = 120}, prekeyKey = ">\1018105\183480o\174879\rB\1023905\NAKF\FS"},Prekey {prekeyId = PrekeyId {keyId = 26}, prekeyKey = " qn7\134867\STX9/\159586\160039\ETB\1042673Py\14900"},Prekey {prekeyId = PrekeyId {keyId = 81}, prekeyKey = "YY"},Prekey {prekeyId = PrekeyId {keyId = 11}, prekeyKey = "3I%\1042824/"},Prekey {prekeyId = PrekeyId {keyId = 27}, prekeyKey = ")\n<"},Prekey {prekeyId = PrekeyId {keyId = 107}, prekeyKey = "\ax"},Prekey {prekeyId = PrekeyId {keyId = 121}, prekeyKey = "\1111508\CANsh4W\b\98481\ETX[B<"},Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "\t\48077#E\a\155471.\154486&"},Prekey {prekeyId = PrekeyId {keyId = 79}, prekeyKey = "\f@&\1111359\az\DC3o\144657\1110414l"},Prekey {prekeyId = PrekeyId {keyId = 81}, prekeyKey = "\144263m\175411WF["},Prekey {prekeyId = PrekeyId {keyId = 50}, prekeyKey = "\NAK\1711v.\177025"},Prekey {prekeyId = PrekeyId {keyId = 42}, prekeyKey = "$\1075344WkY:\v\r"},Prekey {prekeyId = PrekeyId {keyId = 121}, prekeyKey = "J~)tri\178649\994676\RS~G\1088187\34021"},Prekey {prekeyId = PrekeyId {keyId = 26}, prekeyKey = "C4\184672/\146941"},Prekey {prekeyId = PrekeyId {keyId = 32}, prekeyKey = "-\1022334\65623\999008"}]}
