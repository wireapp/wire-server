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
testObject_UpdateBotPrekeys_1 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_1 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 67}, prekeyKey = "\ENQE."},Prekey {prekeyId = PrekeyId {keyId = 73}, prekeyKey = "JUb\34276"},Prekey {prekeyId = PrekeyId {keyId = 11}, prekeyKey = "!H\\I\DC4\99363\30092N"},Prekey {prekeyId = PrekeyId {keyId = 96}, prekeyKey = "\58301DL\"\1047148\SOK\129030\1023642\6043%\SI~\1054456"},Prekey {prekeyId = PrekeyId {keyId = 87}, prekeyKey = "\ACK:5\DC4<"},Prekey {prekeyId = PrekeyId {keyId = 60}, prekeyKey = "\1074644\1030890lG\DC1\1586\1044812VK\DC2\ACK"},Prekey {prekeyId = PrekeyId {keyId = 37}, prekeyKey = "'\NAKS\ACK%\CANg"},Prekey {prekeyId = PrekeyId {keyId = 29}, prekeyKey = "t\26117|A\vW"},Prekey {prekeyId = PrekeyId {keyId = 22}, prekeyKey = "^("},Prekey {prekeyId = PrekeyId {keyId = 88}, prekeyKey = "r\a\1015358k\153909rMJ<\65767\1090264 *\1073799"},Prekey {prekeyId = PrekeyId {keyId = 51}, prekeyKey = "\"\DC4\142709^\aATs\25086\NAK"},Prekey {prekeyId = PrekeyId {keyId = 22}, prekeyKey = "\"x\992264\1096205\ETB\1014910}}\65888\rh"},Prekey {prekeyId = PrekeyId {keyId = 57}, prekeyKey = "\1005366\30354\DC3k#9\1078733/\EM\SI\NUL"},Prekey {prekeyId = PrekeyId {keyId = 94}, prekeyKey = "\1016885eb"},Prekey {prekeyId = PrekeyId {keyId = 120}, prekeyKey = "y\1104941\1091261\DC1h\1045933\r\1063200X\DC1\1074788\&8\121119"},Prekey {prekeyId = PrekeyId {keyId = 66}, prekeyKey = "\1051930:\ACK%H\1066315\a\50610D\147413\SO"},Prekey {prekeyId = PrekeyId {keyId = 76}, prekeyKey = ")\ETX\48046\22237g"},Prekey {prekeyId = PrekeyId {keyId = 31}, prekeyKey = "*\NAK\DC2"},Prekey {prekeyId = PrekeyId {keyId = 119}, prekeyKey = "\SYN\DC31-:\194684\f\1011604"},Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "7r\145218,\42691:\SOH{jVp\990868M"},Prekey {prekeyId = PrekeyId {keyId = 122}, prekeyKey = "#=\DLE"},Prekey {prekeyId = PrekeyId {keyId = 13}, prekeyKey = "A"},Prekey {prekeyId = PrekeyId {keyId = 105}, prekeyKey = "+\DLE\1063255\bZ"},Prekey {prekeyId = PrekeyId {keyId = 61}, prekeyKey = "\ETB\1010113sk[\rxf;\17851g_\DLE\SI"},Prekey {prekeyId = PrekeyId {keyId = 40}, prekeyKey = "~P"},Prekey {prekeyId = PrekeyId {keyId = 78}, prekeyKey = "#\1108882\FS\FSu\"Jf\37282\35600"},Prekey {prekeyId = PrekeyId {keyId = 98}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 86}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 90}, prekeyKey = "\1075383\51563GQa\1104091\1020872\va\1026199\n"},Prekey {prekeyId = PrekeyId {keyId = 90}, prekeyKey = "p\191089\155302\aU\1015243{\ETX"}]}
testObject_UpdateBotPrekeys_2 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_2 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 89}, prekeyKey = "\SOH\142671'\48080gII"},Prekey {prekeyId = PrekeyId {keyId = 61}, prekeyKey = "O"},Prekey {prekeyId = PrekeyId {keyId = 48}, prekeyKey = "@HNNAIAO"},Prekey {prekeyId = PrekeyId {keyId = 57}, prekeyKey = "9jat\1094902/9\1040562\t;xv"},Prekey {prekeyId = PrekeyId {keyId = 21}, prekeyKey = "H\997949\GS"},Prekey {prekeyId = PrekeyId {keyId = 27}, prekeyKey = "\1000685x\1069164\DEL"},Prekey {prekeyId = PrekeyId {keyId = 84}, prekeyKey = "Dj\ETXH\183130"},Prekey {prekeyId = PrekeyId {keyId = 74}, prekeyKey = "E\145319-xF\EMR~?\1075247*;y5\987317"},Prekey {prekeyId = PrekeyId {keyId = 99}, prekeyKey = "X\SUB"},Prekey {prekeyId = PrekeyId {keyId = 31}, prekeyKey = "\155034\&6,d\1046138a]\1085240U\35945&\NAK\RS\1003020J"},Prekey {prekeyId = PrekeyId {keyId = 40}, prekeyKey = "\1087040\1103616\1012077\1047243N\SYN\1029187u"},Prekey {prekeyId = PrekeyId {keyId = 81}, prekeyKey = "_V\1105334"},Prekey {prekeyId = PrekeyId {keyId = 80}, prekeyKey = "3z\97830\US\DC4\\\DC1,\32530J"},Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = "\184888"},Prekey {prekeyId = PrekeyId {keyId = 67}, prekeyKey = "A \69881\66325#\GSF"},Prekey {prekeyId = PrekeyId {keyId = 15}, prekeyKey = "\ESCuuJ-6"},Prekey {prekeyId = PrekeyId {keyId = 64}, prekeyKey = "Tt"},Prekey {prekeyId = PrekeyId {keyId = 59}, prekeyKey = "f4PP\13526|\r[\1056407"},Prekey {prekeyId = PrekeyId {keyId = 56}, prekeyKey = "6_i8\998135y0U"},Prekey {prekeyId = PrekeyId {keyId = 61}, prekeyKey = "I=\ACK\DC4\1011403Jd\48480k\SI\141057\1083940pQ>"},Prekey {prekeyId = PrekeyId {keyId = 48}, prekeyKey = "P\SUB"},Prekey {prekeyId = PrekeyId {keyId = 25}, prekeyKey = "\acn_\162049 \1041335Qu\GS\b\DLE\177898Z"},Prekey {prekeyId = PrekeyId {keyId = 19}, prekeyKey = "5\1100026Z\138733v\STX\SO"},Prekey {prekeyId = PrekeyId {keyId = 108}, prekeyKey = "U\RS\NUL\DC1\97727\1092158"},Prekey {prekeyId = PrekeyId {keyId = 103}, prekeyKey = "K\FS+V\DC4wh\SIgvq\134063~"},Prekey {prekeyId = PrekeyId {keyId = 120}, prekeyKey = "_\1046804Z"},Prekey {prekeyId = PrekeyId {keyId = 48}, prekeyKey = "22\148893P0\bs\59260C\1068130K\1011855\tv"},Prekey {prekeyId = PrekeyId {keyId = 122}, prekeyKey = "\33809\97230%\aCR"},Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "k~\47373Zk\63946Q?\vB"},Prekey {prekeyId = PrekeyId {keyId = 101}, prekeyKey = "\155843\1090396n\17843\&0\v\173978\1006114\fP\52610~\1027409"}]}
testObject_UpdateBotPrekeys_3 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_3 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 88}, prekeyKey = "\NUL"},Prekey {prekeyId = PrekeyId {keyId = 118}, prekeyKey = "\1055573K\DLE*}\DELC\SYN\1007578_j>"},Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 36}, prekeyKey = "OM\1010455m\44400\987108\163859Q@e"},Prekey {prekeyId = PrekeyId {keyId = 35}, prekeyKey = "\10008\RSn\4341\1104111\988000\1028657nCQ\FS\9060\1077253"},Prekey {prekeyId = PrekeyId {keyId = 90}, prekeyKey = "\986991\1016861\US\1093588\SI\GS\140198D\\\f"},Prekey {prekeyId = PrekeyId {keyId = 61}, prekeyKey = "{\152472H\1074208*\59907i\151054\1013790t@"},Prekey {prekeyId = PrekeyId {keyId = 98}, prekeyKey = "\1067020>tX\179658["},Prekey {prekeyId = PrekeyId {keyId = 88}, prekeyKey = "}D"},Prekey {prekeyId = PrekeyId {keyId = 86}, prekeyKey = "\1100715\1111361\&7&\1035187:VCR\48146LW&j"},Prekey {prekeyId = PrekeyId {keyId = 116}, prekeyKey = "\ETBET\\\168119oqD}\NUL"},Prekey {prekeyId = PrekeyId {keyId = 29}, prekeyKey = " t\173156\1015321z8\ENQVJjT)\137393\ENQ\3285"},Prekey {prekeyId = PrekeyId {keyId = 112}, prekeyKey = "\RS\a\GSp\GS.3`5\RS\a"},Prekey {prekeyId = PrekeyId {keyId = 96}, prekeyKey = "h\STX\FS0&\n?P&k\1032482\186018"},Prekey {prekeyId = PrekeyId {keyId = 68}, prekeyKey = "\177897S\44568\SOH\27805\am\1088716\t+K55"}]}
testObject_UpdateBotPrekeys_4 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_4 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 117}, prekeyKey = "3{\67872l\1103102"},Prekey {prekeyId = PrekeyId {keyId = 7}, prekeyKey = "z\1044787f\174646}"},Prekey {prekeyId = PrekeyId {keyId = 8}, prekeyKey = ")\v%\DC2ju"},Prekey {prekeyId = PrekeyId {keyId = 52}, prekeyKey = "H\EOT#\SI\51610S\131331\1067082?\178671QI;"},Prekey {prekeyId = PrekeyId {keyId = 73}, prekeyKey = "\ETX\SUB\111257r;&zW\179129\EM"},Prekey {prekeyId = PrekeyId {keyId = 68}, prekeyKey = "S\1711$BHl\"OB"},Prekey {prekeyId = PrekeyId {keyId = 73}, prekeyKey = "\EMa~by+\EOT\SYN!\1011052\DC2\151296"},Prekey {prekeyId = PrekeyId {keyId = 91}, prekeyKey = "f]\b\142001UJ4"}]}
testObject_UpdateBotPrekeys_5 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_5 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\DC4\1042038.}\EMP{x}\bo\DC1\FS\46642"},Prekey {prekeyId = PrekeyId {keyId = 119}, prekeyKey = "\1095437\125073^DlrY|j\1038543 "},Prekey {prekeyId = PrekeyId {keyId = 86}, prekeyKey = "\SIz\185593TI\33600W\1058279"},Prekey {prekeyId = PrekeyId {keyId = 36}, prekeyKey = "\SYN$\NUL"},Prekey {prekeyId = PrekeyId {keyId = 71}, prekeyKey = "1\2743y\100631<\92666\SYN8\DC4\1038974\US(\100311}"},Prekey {prekeyId = PrekeyId {keyId = 83}, prekeyKey = "\1062516\18337"},Prekey {prekeyId = PrekeyId {keyId = 107}, prekeyKey = "a%}3"},Prekey {prekeyId = PrekeyId {keyId = 38}, prekeyKey = "qDn_\1073155KrX\1012538W\t"},Prekey {prekeyId = PrekeyId {keyId = 42}, prekeyKey = "\38415h'~_6\163301b\48040\1105341\DC4ap3\194627"}]}
