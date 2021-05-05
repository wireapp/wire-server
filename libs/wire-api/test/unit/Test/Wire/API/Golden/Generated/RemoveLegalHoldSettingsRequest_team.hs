{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RemoveLegalHoldSettingsRequest_team where

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
testObject_RemoveLegalHoldSettingsRequest_1 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_1 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Just (PlainTextPassword "\1046086\996855c\SYNF\t)f`3\t\48047ka`GU\1101973\18573z07t\EOT`r\1019954\NUL\1003273\94827X3(\\=\r\1084950\"q\127106O\1028098\t\SOaK~*u\98052\&1\ENQ\EOT\NUL\ETBj\154844-\ESC\ENQ3\DC2\144432f\62368F=\NUL\1076293u\ACK\181079-U\f\167827+\n\1110587\US\38942\97076z#e\995059:0(\44226ij\150690\1015222\&0\1053969\NULa\DLE\148118A\\-\1071493_U\1095309\29546\184002fhw\RS\1043761/fL\188852}?e\EMV9\ETB\2310\1051402\1010879L\180163l\SOH9\f:\1025966B\\*v\DC1'\994913j\72329\&6Fn\55077(|6\174337RBCv\DEL5gD!\1051611\SUBf\DLEQ\5195\bM\DLE\133955\RS\1096617\1065983\1070911=\\0X\50657ts\170735\EOT!\189016\1112007h\1016396Eqd\53699S\166737\RSY?;O\60273\62664\1061405\fb\990530\&7\1028061u\149367,Z+}\EM??]pS\1096624\SO\1089109e\1070667\189724\53995a\1030238\GS\STX\NUL\ETXb \aO\SO5-:\1042410y'\1002790\1000344\1076718q[t)\9885UJ]]\1070116\178821\t\174289c1r\46522St\1052632wN\1102785\SUBf?\NAK\173996\ESC\184493\65207\ESCeMX{E\148860`W\62165w`;P\r&#\145754$\14856RBP\ESC\1004573N\SUB\ESC\"\1046468s\141806,s\SYN\DC2\994006J\DLE\1084969n\NUL\20815V\134853^\179361UP\ACKcf|*3\ETX@d\41767\155772!\bZ\45662UPqC*K\1046396\&9\1060924hoZ'\180100\1002926$J\FS\1070572p~\1038936\1086582\97598$2\f\SUB\1009212\nl\21194\1092230T\fM\ESC\r\1011965<\RS#\SYNR\DC4-\DC2\31414\&2\174511\FS)$\1099156z[h^!-RR\1103399\&9Ys[(\22782N\171705\&4T _gv\SI\1023368\62157\US\60986\\Q\DLE\1099221z\1067939$\1110481@jQ,T|\t\1044297;\SI\RSA\28522C\26581#.v\NUL\b\1087391\983610\aTBa\v|#\1017675\DEL\DC1\1005855!V\t\ACK\ETXWxv\ETX\92997#&,\20103*\38787\73915\1018209U\\1w\1095588\60733\1020590\rB(/\r\95510rY\166369Jx\DC3\17620#\ENQ\993798]\62064w\95540")}
testObject_RemoveLegalHoldSettingsRequest_2 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_2 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Nothing}
testObject_RemoveLegalHoldSettingsRequest_3 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_3 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Nothing}
testObject_RemoveLegalHoldSettingsRequest_4 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_4 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Nothing}
testObject_RemoveLegalHoldSettingsRequest_5 :: RemoveLegalHoldSettingsRequest
testObject_RemoveLegalHoldSettingsRequest_5 = RemoveLegalHoldSettingsRequest {rmlhsrPassword = Just (PlainTextPassword "Y)\DC3eV\3770s\SOH\FS\1052745\ACK:i\999068\DC1|5?\1036219\"\ESC\DC2f9\1038342\183136k\CAN\1084040\1069596\f\t\vs\136772}lw,`\51752\133385{J0cc2|`y_\12993\167357\DC3\CAN3t]\1017092,\153250\1068178i\1051287s\152765\125222\v\119234+0U\SYN\CANwD@&k6yC\US0\EOT(\1049893/|<\1093118\1112133^\1036345\995351\nHu\ACKG+qr1\GS< &ti\17309A\10054s\SOH\\d\46406\1057856\20546\SYN8M\175949c\1078539A\r\EM\175720\&5\NAKQ\155784\SOH\1063179#\1023347\NAK`\170112X\183614\131426Zjw2\1028626\1102325\CAN\NAK9\1088169\v\1030086\128715\1097602\r^\1002201=\24553|Wj\54999\1044528Q\135127o\CAN} \"\1112581\&8a\11189\100379 VP\1031579\GSI\1045685\&3\1010657\1099186\1068674))t4n?\47932Tv\1092550\DLEEr4\120624\FS\158564\50086\1060304\b6\66416'\fxl}X5\"gG\185091\&6J\149110\1095335\1075487Vx%\48350*H\1043535x\94235\DC19\44296/\1093553#e\48213.v$Z\995776M_\95717|l'l@JruK\STX\1073378\1070942\EOT@\SIN\681\&9\45424TO\1040447\145434\991943\CAN\1020696\USp-\SYNe8\US\FS{.c*\a<n")}
