{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.DisableLegalHoldForUserRequest_team where

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
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_DisableLegalHoldForUserRequest_1 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_1 = DisableLegalHoldForUserRequest {dlhfuPassword = Nothing}
testObject_DisableLegalHoldForUserRequest_2 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_2 = DisableLegalHoldForUserRequest {dlhfuPassword = Nothing}
testObject_DisableLegalHoldForUserRequest_3 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_3 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "\1038170%\RSjZl{\SI\DC1H] \ETBx(\190279{z\SOH\"\SOH.\n|\35401\EM\144921j\\/]_*\SUB\\\996512bm!\1044355hw\ENQ\DEL>\983545m(\184524J\1053314\&2\".y\1043021\SI\1034149\171938\DC3\70410\1083212PK\SI\1060679\&1R9\1113484\ETXgtM*\ETXlC\38156\rIr5G\SI\n\ACKbk\178261\49283\1088681\1068234\142385-\STXFr\1091868K!P\1009367\40076_m\59005\US>[VDpG\ENQu\bRgKI\EMt!s14V\992243T\6551\&0R\USrZq\191286D\1080120I\SIg\1108604\158547R\a\53464\32472n\1081699\1108080[\SI[l\US\100403$'W\1109268\tQ\159229\1054490QH \SOH\USGtz\r\1085008\DC42\r\7908\DC3h\f\DLE\20983\NAK\22716 .\EOT\SYN\1050546\1013684\RSG\74224u\1025853C%R\ENQnmARaG\99848Fa\129406e;\US\SYNLqb\991706;;;2n\4148VaU?-\SYN\1055411N\DLE\DLE\16214I\NUL\157501wD+G\DC3\CANN\169907P\997808\SO(\121298\&8jt8|\49636\38615o3{\1066848Iiv\1012996$\n\t:B1n\1010928\146276\ESC\58551\&0M?\50129M\1105829-!\DEL\FS}\58701Er$#@\1056574Pp\1097477`}X\STX\6771\&7Xn!\nqF\1087271\GSK`\SI~\CANZ\4504pY\194709\STXN\1024596\74360\1008842=\95693\1101721z)\ENQHnZ4 \1114104wI~\1052597#}\150277V\177038\1078753n")}
testObject_DisableLegalHoldForUserRequest_4 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_4 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "+{y-@=E\171454:\FS[BU\34829\1099123<Bw6}<\1070634o\a\7340N2\ETB2\1059452\1073558-\GSxr`\70825*\nt\191449$\1076331\DC2\54641\985415-\23201\FS\SOHI@>\1076773\1072785\ETBg=\1076890\58509?\"\173673s+\174326>\995342\tm1n\1034472bH\US\ETB\8594\\h\ACK\126988\ESC;\1050496\1029404\&1\1068263;}y\1017448c\b\NAKV#\DEL\1028575(EUCC\EM\183323fm3W\59851t\EOT\98638F2\33920\&6\1102511\1062342C,\133595\181632\51714\"\33104Z{mpee\EM\1045102\SYNl\DC3H\993792\145139E~\140219lbeA\47259\1026938S\rN2\1030991!f\1096448\1088825L!\1076348\&5>2n\DC2j\SYN&\93806Y\1025672\987888T'sJ{\1060194\ESC\1081818r\DC2ll(\a2O\r\31594\ESCH\52186\60941\EM\ETB#\1055651\DC2\1065341\NUL\168035\&6\1101467r\1032413c'9\1070811\ESCf\SIXp!\aQO~G\a\991644\1003854:\SOH\EOTS\983415\14056y(\CANM+7\1019621\62358\3305\1002354\62535\19259\DC1k}\182544R\b\154102\GS\NUL\1109324\USJ\1097268Kc\1088831c\SIX\SUB\SYNr\1068938\NAKXV\151210D0{\1057716[\DC2_K\51597\992580@\7227\1020294hr/\991633Qc\42272A\73069+\1107181\t<fuN\NAK\38788U\1064621q?\177165\50394'N\1095782g\ENQIX}|\990736\SOLV?\1010465\1110167\1061310z{\149535?\"\DC2\\ +nfwN\94652sC'>K\46560\NAK\1021657\40549\EOT\1054654\DLE^*d\ESC\1083861\&5\ENQ\EOT\52406\30762\145895\SYN\97187_\36294G07\38488Xc9G8+\r-w\163158\1014501\&4\1085193\DC2h\1106639\&9*\21629\nJe\29046sj\a\58409w\ENQ\168915\ACK\DLE\DEL)3\53625\RS\1093954V\68491,\1504v\ETX2\16993~\161859\54190*\bx\ETXv4\v4\1087636\"\1023807\DC4V\141271m\DC1\1088264\SI\50480lGzrV\CANv\n;\1014023\154382/\t\31171\1014872\n\1032181uP\EOT\NUL\\\DC1\EOT\DC1\v<h\148496\1002713G\US7\t=\SOH*2d:\EOT D\DEL\194993$\146257\DC3\1113190\1031568\r(o\136366\174588\ACK\33215% zPce\NAK\ENQ\1050360~J\1085695\NULmg\"of}y3\25823?\1010782\SOH\SOrh\157839z\69952\1074069\fK\68617<%\1059355\22368g\1068795\20606\994337\&4\1104121\&5\1114043\&2\ESCr\1031137&\123162\96459\CAN\ESCo\1083555\ENQk#\1097300\998686\67704{\44437\996843\160414pP\146453g!\"\174959VK]G\ETB\NAK'?\1076718\&7\SYN1\r\1106550~$\\w8@!}")}
testObject_DisableLegalHoldForUserRequest_5 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_5 = DisableLegalHoldForUserRequest {dlhfuPassword = Nothing}
