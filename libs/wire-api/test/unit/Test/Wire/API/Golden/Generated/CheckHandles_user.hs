{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.CheckHandles_user where

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
testObject_CheckHandles_user_1 :: CheckHandles
testObject_CheckHandles_user_1 = CheckHandles {checkHandlesList = (unsafeRange (["\FS\44288\39042\161689^k\32363\&1","FgO\1060612\179661\156772\EOT\1053915","\1049270\71710B9Zi\DLE","\18999","V\n9C3{\1030369\DC3","\134909O\NAKOf","Mh\22449B])\v3\42209\&8\EOT","%\15176\&8Ze\1047481}","\DC2\\\\)\STXO\f#~\31188","\189587\8332T","\1009568\ESCH\7612^\1003844z\US8#>l_T"])), checkHandlesNum = (unsafeRange (2))}
testObject_CheckHandles_user_2 :: CheckHandles
testObject_CheckHandles_user_2 = CheckHandles {checkHandlesList = (unsafeRange (["","\v\1027678wdG\160113\990052L\FS\176948\&3","\180784mtzg","\FS:\1019942\a4\SI|VD\1068747\ACK\SI7\NAKM","|\GSz\132908\vA\1073338~\64541","R","\1068615\1054757k\128318r","K%AL\t\CAN\1102955","\NAKR\"Z\STX+`\DLE6\GS\5636\83416c","","\RS\1065066\74779","Rx2","=\\G\SI)"])), checkHandlesNum = (unsafeRange (5))}
testObject_CheckHandles_user_3 :: CheckHandles
testObject_CheckHandles_user_3 = CheckHandles {checkHandlesList = (unsafeRange (["\SO\CANu\1030506f6AS/","\GSaR!\bd\1075968\&7\39601\&85CJ","","u-","W\SIjTx\120605oG#\53464\NUL\1076726x","\EM\45562\ACK[\78172+\ru\NAK\DC48\1057563\DC47","\t\1004920\98099'+RV>R\998147j^\157632","G\1003921\1075965k!VbB","\1002010\24812\133855w\132246{H:9\162215F#\EM","6\1034996\1056077\157451\1033804\ETX\166743O\NUL\175727ve\10366\ENQ","P\DC4f@\US\47768\1105707\986429Q\97241>g","\1066061","q\r\19592ore\DC1\CAN","PA[\1082158V\97043Xwj","J\ENQp\1057248,\20726\1015135\26841+\FS\ENQ",";EIu]_\181279_\DC2`","Dn(\33426\1034173$g\4364\DC3v\181910\167877","\1042627\b\ACK$``U\98975\&9{\48970","%\FS\1088265_\DC2\1023631@zX\61198l\r\1066435P","%H\50903\&3Ev",")o266Kt|\SOH","\72826\21515(]N\"","\170630\SI\1018046;?\n\182993V\152273","P+\EOT[\134806\aI7\1088321\DC2t\163339","\STX)\US\SUB\SYN7ushg\ETX\1060174b","",":X\v\149940\n/b\123176vPp`\r\DLE","2\1002428\1064266rm\6622\167663\1098140 ","8\NUL","x\134144%\1107315\50035P\EOTh\63568`Q\1018224@","r\1010353\SOHT\"\FSci\164894\1058289\15967","r","\139829\n\SYN","\141598\52941\1019246E\CAN\163772f/\SIo\1023575\&97\b!","C"])), checkHandlesNum = (unsafeRange (9))}
testObject_CheckHandles_user_4 :: CheckHandles
testObject_CheckHandles_user_4 = CheckHandles {checkHandlesList = (unsafeRange (["H\RSY\SUB?\49003\30064\38372\&1","v\STXC\NAK\EM\1048906","g\SOH<\USJ\1094304*","\987640\185427\b\DC1P|\182162\DEL\ESC\t-zM","","@G\DC4\61230K8\SOH\\(eq\18756^","#|:\1000991.p\1034601qG","Z\1108803\t8L\ENQGv\54168tB+]","\SUB\ENQ$\v`\bk\DC4\141892n-5-j","'\vqO\RSD\SYN4K\ETXP,","\EMsw\1075638\ENQ\DC2\DC1\CAN\FS\ENQ\1019300","\1024797?=\FS\v\NAK>\ESC)","\1025658\DC3k\EM\74017\149083\156513^\DC4\10613\NAK0}\7899\\","gK\139475za\152679UNq\36144r\ETB\1096817","t\757[\1101436[\ACK*Y\ENQW\DLE\142253","\DC45\55007","p1xw5\1080247\CAN\58728b)n\v]B","\n\74314\57763\STX\1101880[1*\47271","\NAK","\DC4.\RS","\EOT\172020{7L\FS","?zY\SUBW\DC3~W\GS1\166340t%\a\173651","\SOH&o\167640\1078232w","\1075221,>x\990925-k\GS\66643\997232\28371&","E\172609.8V(\GS\993671t\"\158086U)\SYN","u\29506J\141557\US}bw","\1055097","j\t\988839\174207'=\GS\162858","K1\nGS\\","$"])), checkHandlesNum = (unsafeRange (9))}
testObject_CheckHandles_user_5 :: CheckHandles
testObject_CheckHandles_user_5 = CheckHandles {checkHandlesList = (unsafeRange (["\1013264e\NAK","\EOTL\72192;a)M\58756 \NAKmyk","","\142839\US","\SUBPL\2380\ETX5L\DC2\SOHc\38443ugT","\1110961\SYNM\38838\1039990`T\ETB\DC3\EOTT",")1\NAK'*\51237S \FS\NAK}\DC1\ETB","\1027894\26631\33489%'\DC1V\1033360~-\1069744\12425)s\n","s \1113166\RS\1009617l\12294[(\1091273Y2*9","\1113992\133811","m","{\1070392|se\SI\995243S\25987\DLE","ux\SO\1049733J~e","h%\CANB\61887\SUB\aP}\1056214\187390\1086588%\ETB","\SUB\1111812v&","\149809\DLE%\ENQ1\1028001\ESCBU","a","P\1081285\RS\1029967\57854\1106071+ \ESCI","q&X\118935t\1108567O6\b>=eG\CAN@","\US,9\USi\n\ESC1b]\DLEsk\984532","\134328\DEL\985298uJF\42025/P\1041977"])), checkHandlesNum = (unsafeRange (6))}
