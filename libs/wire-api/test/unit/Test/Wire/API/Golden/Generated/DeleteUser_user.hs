{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.DeleteUser_user where

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
testObject_DeleteUser_user_1 :: DeleteUser
testObject_DeleteUser_user_1 = DeleteUser {deleteUserPassword = Nothing}
testObject_DeleteUser_user_2 :: DeleteUser
testObject_DeleteUser_user_2 = DeleteUser {deleteUserPassword = Nothing}
testObject_DeleteUser_user_3 :: DeleteUser
testObject_DeleteUser_user_3 = DeleteUser {deleteUserPassword = Just (PlainTextPassword "v\aDdW;\1091247\SO\CAN%\1060534-u@\1003333\98947\GS\r~\172819c\SO\1019681O\\\CANO\EM\t:>.y\1043427\a;W\b\1113402%\1082328\21620\&6wh\DELq4pe\1024326\CAN\NUL&WnrWm\1114103\NAKMl\1012630\1013690a\1104998(`$x\43204\1017315=\FS\t0I~\SYN\1065352!g\1016134f[\165440\a\SYN\SYN\\H\1074829g\SO\a)\1090079\1060985}^\15497$BuT\11383~l\1089639~4x\GS-!!}lG,de^\145717<(H8R\1111757;/\1051274\"\DLEl<E~!\1010672!A\12875@!\31021?`\DC19\DC4$r\153379\DC4\vhkhl\1002176Gr\41185d?\1051236\1041065\141383\&6eE\DLE\984976y$[qC")}
testObject_DeleteUser_user_4 :: DeleteUser
testObject_DeleteUser_user_4 = DeleteUser {deleteUserPassword = Just (PlainTextPassword "xk8|\\h\988513U\1063906Y/I\151951\139188n\1043091\EOT$X\SUBOV\179878\SUBhN9\ESCq5\172063:{h'H\45215\178392X\ACK<Fp!?x\"\1492~\CAN`C%K\t/\1104184\ETB%\1101053\SOl\138242c\SI\58983?8\r\RSo\1078035N\161437\ENQ2u\155468d\ESC!#U@%yi\a\t\169745Wv\1074252\USjD]*\EM>ID\7456\1024416~,#?\137005D\185744/\13841\ACKZ\156988\&9Y%\ENQT\t\tY@R\18642a\149413^lZ\1031435qL\ESCMC3a\RS\1083028\NAK#\SI>.\999627z9\985262K\135771|\v@{7\t~5\128320Q$6{\31836\1059403n_\5272\ENQ\CANOd\1071018\1041302$\63523\121316\n:\119013?\ESC\1095365\1007138tw(\1007400\1063592\NUL\r/N\31448m\1015844/\rJ]v6\SUB\f\1009958\ESC\1111575lV\1009269\FS\SUB5Wm&)$lL@H~?U\EMPp<\185152\143261RT\SOH\EM\1070612\DC4[;.\1111726RU\166240'D\DC2hB\19849_K\26772w\tgmM>=vQ\ENQ\1086598Bh\DC4\1039170Zf8y!r\1073420\1051603\1045842\US\1001771\a7Jo$\1055667Ax\133011\DC3:;5(9_\136166FN_\tp\vd\1093087m.Xr\1051552x*\r\1040739oh8\ETB\1003808\a-P\37209'\fi\59424H!/\ESCF\\xw&U@\45216\&5\1031007\985786'\SOH7{\1068252i\ENQA\1055979I\999994\156756\1051532p(\\\ENQz\988777\1102372hy}\188839\1030722\120671+D4\46481ijAd\1049715JM:\1008011\161437\&00\1061884G\DLEX\40079T5Y\70471U\22085,db~#:G\"-6L\986649\78711\136993\CANUz\DC2\1031758-=c\RS\990746xMh\1002208-\DC13\41289FJ\t\135106q~\1022563\r\1102300~;!n[\139364^\NULj.")}
testObject_DeleteUser_user_5 :: DeleteUser
testObject_DeleteUser_user_5 = DeleteUser {deleteUserPassword = Just (PlainTextPassword "i\EOT\16526QU2\ENQ\42466C\179580c\SI,hX\6051RM\US3PDiy|N,^\917565\986376\1057587/R\25175~Eusl:\129086\SIE\145287\&4\DELQ\10237\110742\US\1046350N\DEL\a(\74571\140340?`\163079\EOT\49139=?\120904U\1049200TZ/W\13380tJx\1002970^\121471R<Fw\162872@;\182794\ACK\1074811]\1027440\ENQ\1076867\RS*T\1094877C\17021\"4\100920\ETB1\1069227@\17198\&2_\7394\EOT\1031992I\65608r\f_Oz\995721T4m3\STXa##`\1049359!O\r\f#`\1074330\bC#0\1005464W\1045596RuQ\1028974\&9\STXvBp\188331\136749,u\990844\&2\SYN)g\DEL\f\US\DC1\DC1|!\1008675\f\GS?c\1015585a3\144476\23772\&1")}
