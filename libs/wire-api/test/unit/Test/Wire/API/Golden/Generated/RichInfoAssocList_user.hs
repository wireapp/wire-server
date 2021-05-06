{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RichInfoAssocList_user where

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
testObject_RichInfoAssocList_user_1 :: RichInfoAssocList
testObject_RichInfoAssocList_user_1 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "`\rD\1069069;Ga]\1027997w\27254\EOT\41438D\1059387TKo\1034953\1010731W", richFieldValue = "\1043832\NUL\141695\SIf{k-\22552\1083596J\1080071i\1083371l\DEL\1012180"},RichField {richFieldType = "\14553S,Yj\1109089\152596!F\DEL\NUL", richFieldValue = "\bd\92672\1000463d|t"},RichField {richFieldType = "r46\128022\ACKg.1\1061319R\184907qb\t>SP\175492\1076627,Hx\SIt\1036242\"(", richFieldValue = "\997084\SYNb\CANCE"},RichField {richFieldType = "\1111159hl\1088692\r@?`-6\ri\SOd<\a\1093656", richFieldValue = "pT\145543?`\f0\82965O~\1070703?`\137177T!ko\EOTvG2\155633\184346\95379i2\163502\SI"}]}
testObject_RichInfoAssocList_user_2 :: RichInfoAssocList
testObject_RichInfoAssocList_user_2 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "\94663\33547\1054056OP\78636\&2l\998812", richFieldValue = "T\EOT\ACK!\1094633\&2\DC2<\94795c\1056\v"},RichField {richFieldType = "\1001099C\36685d\DC4\SOE2[O\v\134717=~Sn(r\131357", richFieldValue = "\170605\1019647m\44951\1065510\1074455\CANh\vUR0\RS\176643K\24610\&8\1049585\DC2\DC1\US\DC2?\a"},RichField {richFieldType = "\USLwL\reo\SOHH2\SO-\SUB?=\CANV$", richFieldValue = "\35068\32730\1086857u"},RichField {richFieldType = "i\1062482tr8\33108\1056143\SOH\ENQ\DC2:\a-\990651\158988\53736\ETB2(y\DC4\78825v\ETBt\ESC\DC2\ETX\4263", richFieldValue = "\SYN\1049183\&8%ku\1041250\&60r&N"}]}
testObject_RichInfoAssocList_user_3 :: RichInfoAssocList
testObject_RichInfoAssocList_user_3 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "f4", richFieldValue = "\24559{f3\1082231\133071w!HG\153525\EM5F9z"},RichField {richFieldType = "\71066\174635\1096897\DC2\1071974", richFieldValue = "a\1071044\92293"},RichField {richFieldType = "?2j2zgF\1110372\1064381\1005129KT", richFieldValue = "p:oA \DC1\DC1f\151218g\1016460\1004867\52643%}p\1096856\137100\173158\47933\1041204\EOT\29686Pw"},RichField {richFieldType = "\SI\61687\60259b\DC3\996055", richFieldValue = "td*\EM\RSi\155760\160311_\CAN\1013010\USf\10379?\1054815\1095837\1028048\45096\GS\STX\SO%F\1057551P\211^\1071675U"},RichField {richFieldType = "\24543\182853F \140539gN 8q\DC4I/uVs\33821Qr\8541(\24628rNU\92415\&3zR\41725", richFieldValue = "?0h\r\138453\53849\STX\1032077~\NUL84EM"},RichField {richFieldType = "\DC1L%)P`oo/", richFieldValue = "d\151641p\"\EOTJ\48656_R}i\1043522)Ne\985302#\SYN"},RichField {richFieldType = "\51815", richFieldValue = "m\"/\rQ)G#\a_\137959-\984243\36644C8k\1017175f.\CAN"},RichField {richFieldType = "", richFieldValue = "lHiM[\ENQ\1112307\DLE8\992610,Ae\1001627O\23927\51593"}]}
testObject_RichInfoAssocList_user_4 :: RichInfoAssocList
testObject_RichInfoAssocList_user_4 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "]\190163", richFieldValue = "+J0\1027659]\44393\1016011iPy\SOH7y,EuB\vr\ETXW\1061739\SIM\CAN\US"},RichField {richFieldType = "g\CAN!S(Y\1058557\NAKV]B\98755\DC2.Zq\SYNV", richFieldValue = "\157879\1107745\1009193\1032668\SOz\1059259\rK\b\1107256\&9\1109999<-w\v\SIJ\166656N\ETB"},RichField {richFieldType = "\1002834`'\DC2\1036181x\135429\1061820\FS|l\SO\188636\160193[v)S\20475gs\41346\&3q\1054674", richFieldValue = "<C\996265S$"},RichField {richFieldType = "\CANJ\1024649B_L\1006286pl\\\164769\DLEDn\65515%DWz\42500B\FS\f\141927\146753s\1061991", richFieldValue = "\1106765y\US\94944\t}F*z}\\"},RichField {richFieldType = "Eh\1097225\SO4S\FS\1035402\DC4t\1049601Y$\ETX\16855\1092513\1036589\v\1102019:\ETBjd\110983\1065175\1068962\STX9", richFieldValue = "n!\DC3\DC2\1058287\1002624'\t\992926X\SIvu\25148C\1090375\20128 \t\73957\vk\1105025L:\68372\47500+"},RichField {richFieldType = "\1017764`\1028719\b\991148+n\1093081&Mar\132237\162474\rmDq\DLE\ETB\1035791G\186180\1090125-)", richFieldValue = "\1080222J\EMV\1015782\1090866\1070411\ACKNq#\6019\1001380P;^\NUL\70724Wgq/\EOT2\1065429\162689PW\n\22274"},RichField {richFieldType = "\1104393\&7\ENQ:=\1003304\"CR3&\DC4\1099080", richFieldValue = "zo_\68773\20179\ESC\1011539\234`1X\1029350\&1\b\t\DC4"},RichField {richFieldType = "\195049\987186\19327\1107970+L\13376!#\1052502z\1018636\1021921\ENQyh|\ACKg7<k&S\1032002\1111360", richFieldValue = "7\DEL\DC3m#"}]}
testObject_RichInfoAssocList_user_5 :: RichInfoAssocList
testObject_RichInfoAssocList_user_5 = RichInfoAssocList {unRichInfoAssocList = [RichField {richFieldType = "Hu\ACK\NUL\NAK\GS\ETB\STX", richFieldValue = "+\148012>"},RichField {richFieldType = "\1041832\v\DLEM/\133338\1091@\ENQH\1005516\f,*^\1044733\NAK\1028376\1050529]t\142558u\1068466H", richFieldValue = "8!\988519+"},RichField {richFieldType = "\ETB/Hr\158832}?..\178775$>`Y\133247(}\DEL\"\1014051\&6\50093\EOT\DC2", richFieldValue = "TM\1260]\99444lV/> $p;'\\jc.\26299\175969{2"},RichField {richFieldType = ")hK\FSB&\GS(\43008\&2p\f\FS\59890O\993359\t\f\992089C\1096654\EM\r?", richFieldValue = "\1046042\1057728-G\1072272\aW3W\128838\vQ"},RichField {richFieldType = "\1042428/$\SOHv;&5r\DC4\1006569j\RS\NUL\180476A\134628", richFieldValue = "\r`:\vGDTg\69742p\1068626x\24513>\"|"}]}
