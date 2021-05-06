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
testObject_UpdateBotPrekeys_user_1 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 106}, prekeyKey = "\US"},Prekey {prekeyId = PrekeyId {keyId = 118}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 61}, prekeyKey = "`\189876u\183882\70194:\4613\CANch\ENQ"},Prekey {prekeyId = PrekeyId {keyId = 66}, prekeyKey = "j\ETX\142448P"},Prekey {prekeyId = PrekeyId {keyId = 60}, prekeyKey = "\GSGr-\SI\1069736"},Prekey {prekeyId = PrekeyId {keyId = 15}, prekeyKey = "l)?\987639\143055\157743\&7Mq\ETX"},Prekey {prekeyId = PrekeyId {keyId = 65}, prekeyKey = "B;\1082759l\CAN\SUB"},Prekey {prekeyId = PrekeyId {keyId = 15}, prekeyKey = "wX\150753Y\163150\&6\SYN"},Prekey {prekeyId = PrekeyId {keyId = 24}, prekeyKey = "vA\NUL3a\45230\172427t7o\NUL`\993778\23673&"},Prekey {prekeyId = PrekeyId {keyId = 119}, prekeyKey = "l\EOTR&\1038657m'\137045"},Prekey {prekeyId = PrekeyId {keyId = 32}, prekeyKey = "GR\1003547\189477"},Prekey {prekeyId = PrekeyId {keyId = 52}, prekeyKey = "m>\35633\"m\DC2]\58973\997950l\ETBT\US"},Prekey {prekeyId = PrekeyId {keyId = 92}, prekeyKey = ";x2_(\1069839\1023946<@\vC\ACK\ENQ"},Prekey {prekeyId = PrekeyId {keyId = 106}, prekeyKey = "uXo\40151\179022"},Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "Ly"},Prekey {prekeyId = PrekeyId {keyId = 12}, prekeyKey = "W"},Prekey {prekeyId = PrekeyId {keyId = 21}, prekeyKey = "$C\tE("},Prekey {prekeyId = PrekeyId {keyId = 85}, prekeyKey = "\t\1056318\SYN\ETX\1067236o\985669"}]}
testObject_UpdateBotPrekeys_user_2 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_2 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 58}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 117}, prekeyKey = "\NUL(\1071451o\b@\GSko\101036\GS\"\1029226c"},Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "m\1066322\&6G;\GS\1050997`~;\1026684"},Prekey {prekeyId = PrekeyId {keyId = 82}, prekeyKey = "'\179127\&6\182251Y"},Prekey {prekeyId = PrekeyId {keyId = 39}, prekeyKey = "\DC4\12905"},Prekey {prekeyId = PrekeyId {keyId = 100}, prekeyKey = "\1060563A\DC2\171977\41104B\129288\24966f"},Prekey {prekeyId = PrekeyId {keyId = 45}, prekeyKey = "F\1099791\&9s\NUL8\t:D-\75011\1047181"},Prekey {prekeyId = PrekeyId {keyId = 48}, prekeyKey = "}\1007884\1010786\1047494\EMz\1003401Xa)\DEL"},Prekey {prekeyId = PrekeyId {keyId = 70}, prekeyKey = "\DC3F\ESC\GSPve:="},Prekey {prekeyId = PrekeyId {keyId = 57}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 112}, prekeyKey = "\997125\1042150.,O\1062603\1035998Js\SUBc\DC1;"},Prekey {prekeyId = PrekeyId {keyId = 114}, prekeyKey = "n\ETBe\ACK\ETX\ETB/\ACKa"},Prekey {prekeyId = PrekeyId {keyId = 19}, prekeyKey = "fjx\r\1103646b\ENQ"},Prekey {prekeyId = PrekeyId {keyId = 108}, prekeyKey = "+\RS\CAN'\156811F"},Prekey {prekeyId = PrekeyId {keyId = 33}, prekeyKey = "\1053440\SYN,\1106995\983346\FS\SYN\"\SI\r\1039376\21929[E\DC2"},Prekey {prekeyId = PrekeyId {keyId = 27}, prekeyKey = ":B"}]}
testObject_UpdateBotPrekeys_user_3 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_3 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 45}, prekeyKey = "N\96870{Z"},Prekey {prekeyId = PrekeyId {keyId = 15}, prekeyKey = "\1032661z.^##\28378\1072832HL\1079915\ta"},Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "\ETBUf\DLE~\v\DC4K7u\ENQI\FS\1069351\&6"},Prekey {prekeyId = PrekeyId {keyId = 12}, prekeyKey = "|\DC2H_,\NULb"},Prekey {prekeyId = PrekeyId {keyId = 51}, prekeyKey = "\SOiM"},Prekey {prekeyId = PrekeyId {keyId = 61}, prekeyKey = "\NUL&\DC4\1036887"},Prekey {prekeyId = PrekeyId {keyId = 6}, prekeyKey = "\38314N2k\\"},Prekey {prekeyId = PrekeyId {keyId = 27}, prekeyKey = "\990356m\100984G\32479Q^5"},Prekey {prekeyId = PrekeyId {keyId = 99}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 114}, prekeyKey = "\41727\ENQX\ACKD9{!\n\NULQ\b\"\986611"},Prekey {prekeyId = PrekeyId {keyId = 71}, prekeyKey = "~\994256R\ACK&>\58666~\NAKx-\1099507s\NUL"}]}
testObject_UpdateBotPrekeys_user_4 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_4 = UpdateBotPrekeys {updateBotPrekeyList = [Prekey {prekeyId = PrekeyId {keyId = 71}, prekeyKey = "\""},Prekey {prekeyId = PrekeyId {keyId = 107}, prekeyKey = "\DC2"},Prekey {prekeyId = PrekeyId {keyId = 36}, prekeyKey = "\nj\36153h\1057802"},Prekey {prekeyId = PrekeyId {keyId = 66}, prekeyKey = "[\1086884\984034\t\159807\53016;:\FS\STX\39626v"},Prekey {prekeyId = PrekeyId {keyId = 10}, prekeyKey = "O$\1007071\186499"},Prekey {prekeyId = PrekeyId {keyId = 48}, prekeyKey = "\a\1080384\US\65241\SO5iT\fWj\NUL"},Prekey {prekeyId = PrekeyId {keyId = 69}, prekeyKey = "-!\tK\1010174\USO\141701o_\1072460\1035789B\1037753"},Prekey {prekeyId = PrekeyId {keyId = 79}, prekeyKey = "\1010384\1102806r\a\1030353\1040197#\ESCZ5\SOB0\43270x"},Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "\aQlm\ETX\992843O_v{$"},Prekey {prekeyId = PrekeyId {keyId = 49}, prekeyKey = ""},Prekey {prekeyId = PrekeyId {keyId = 50}, prekeyKey = "\1102753"},Prekey {prekeyId = PrekeyId {keyId = 30}, prekeyKey = "eD\174243\&3d\DEL<#\12221"},Prekey {prekeyId = PrekeyId {keyId = 68}, prekeyKey = "\1041310\DC4ls6\62888\&0\1020541/2v#<\1069587\18454"},Prekey {prekeyId = PrekeyId {keyId = 23}, prekeyKey = "#-\"\US\1087929\36708\&5y)]"},Prekey {prekeyId = PrekeyId {keyId = 62}, prekeyKey = "\997710/\ETX\ETX\41406\15427,\1053360\21558"},Prekey {prekeyId = PrekeyId {keyId = 78}, prekeyKey = "j"},Prekey {prekeyId = PrekeyId {keyId = 11}, prekeyKey = "~"},Prekey {prekeyId = PrekeyId {keyId = 70}, prekeyKey = "\23620\&6\994390\118921\40005\EOTtf5:\RS"}]}
testObject_UpdateBotPrekeys_user_5 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_5 = UpdateBotPrekeys {updateBotPrekeyList = []}
