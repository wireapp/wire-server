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
testObject_DisableLegalHoldForUserRequest_team_1 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_1 = DisableLegalHoldForUserRequest {dlhfuPassword = Nothing}
testObject_DisableLegalHoldForUserRequest_team_2 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_2 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "\32797v\bEA\166971?\1016868\&23e\1087992\ETXZG>i\135270\23075#jc\164449\152528=\1017219rN3uRt\58036(rNQw~P4J\1004213\&3\164233\994554\&0Z\ACK8t$i\100575`")}
testObject_DisableLegalHoldForUserRequest_team_3 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_3 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "\NUL\t\ESC\ETXkk=\13596\&8y_g^\SOH\SO\t\1076565\1037301!v\DC3P\ETBb-\1016571\189908\&0\1089431\DC1N<C\181410\1109549O\183114r\DC4\DEL\CAN\48066!\1040463jlHz,\GSDI\SOjn-\n@\\\GSC\n\ENQ\ETB\94603T\EOTS\1060427\988392\1012038yD9\1103400\\@\1085247!-\1074312G\136566\1017384\&7\t<\162760\ETX!\r<\16862=\61752P\139674xc\\vG\141411\SI\STX\996733u6\NAKto\1001931\&0\GS_\ETX\SUB\DEL\11073#\DC1\181178_\991148\&0\165148\990823\28489\1000197\STX(kF|\155506\1057980N?\ESC\2380\16492`*\1085416K\1073976o3T\136696l\53146\DELp)v\1063770\1068142\SYN\72749`7\70019\179428.0\1110752\169213\161041\43036T\1015956\172782gB\SUBWa\1012783,\DC4<-7\1043825\DC4\1080363W0\49464o\10949\1097237E6I\USu\1077980%\RS\EMk\tTLw~\1038565JIC\SI\1063630yOT\CANJ\1056983\tq3E\175915\US\1064278\DC1RJ:\DC2\1076844f\154251\194732\1056875\1067034\28337{mP Wmbg~\"5\rQD\43745?Ow#~[L\993676\EOTT\ESC}\1062811\ENQD/I\181720\NAKD\1106985:N\1079546\GS\NAKTH\1094228;\1068094:u\DC2^\1091092\a\SOH\41435Cx!\154686\SUBFA\1045615\129536 M~<Nl\r\SOHtxD\ETB")}
testObject_DisableLegalHoldForUserRequest_team_4 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_4 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "A1\1012680\59087$3l\1091327_\174375]DvYL\CAN\USs\1084949\1015972\1069281s7\26725\169737~\ETBY-0uj\1103118o'opR_\n@\1103387l/\GSO\1093646\990157\DLE^\70025,a\b\DC2\139762f[\1030007W\STXF\182979\1111247\1052441\SOHg\1071990\30823\fJ\990104B\137510{\r(\49999\1090164\f\157243#\140798X~\ENQ\983819\&4ah\r5\62176\1064121,FG7}\1032680\121042\1020737!\24932=W\92507\SYNW+\DC4vZ\94716\1090868\DC4\144893_\94702>IQet\US\ESCr \\\EOTR}\CAN\NULQd\FS7Rm./\44947Z\169456K\NAK2#{a+\43968N\1043233\&7\71074\1091352\RS\RS1\ENQ=\DEL\26838\DLE\133522dDL\151160?\169641\EM\985699\1113364\68341\&7\SIJaahz\SO(3_0Y\1109496oy\158158S\1097621r\1110617\985780\SI\DC3\4106!\DELrtK\50077\STX\1023397\999185%j\1053200\1096630+mx\993213\45113\1089217\156505}oq\DEL\"\SIa\DC3\rhA>\188421<\SYN\151666\132176\97740?\t\t\DLE0&!8)Oh\149262\DEL\ACK\1055873B\SUBZHw&B)V\1106019\1008235\SOH\132469\47193f\SOH\ENQ\SO\DC4(b\DC1r\42431 Yo&\GSJ*rw\47461\1053924\161775(\28950TJ8\1021969\49865\174745Q; 6T\157016\1058550\ayF\DC2%\142469\RS\a\24443\20100\1007887b\1087161V\DC1\nVM\SUB+\985516\SYN\n\a%$DO\SI/\129145y~\RSH5-BL\1013168pC\178536\aH\1017889B\CAN\1064524\EM<\ACK\1093855\1053820\99901\DC1\1083460\51086_F\DEL%\DC2%Do\DLEEG?\1007732T\172345.PI\1099056\ETB\22543ZG6#\DC2\1036714\1061441\&6\NUL\\\ETBh\1038431\183521\1089143\173510\63361\1083380(D+\a\DELk1\SYN\158646Je:'\n\1099260V.S\DC3\ACKE\v\74507\397(\135984\SUB/4r\NUL\\D\1095706e\1055351\33670h\174961\&4\983499q\78103\1055278q\31933&zVrm&l.\1060848\142062Hdf\"G)\\\SUB}\95811XH9}\147722\36464L\SUB)9\1110921\GS\1074166\&5a-\ENQ\50410\EMN\GSk{\1105928\b\870%+\DC3\a\1099373QiFTu\ETB&\US;\173871^}a~f\58276FF!\US\39149^\11975$\1053359D\DLEcs<N\ETX\GSp3!\1090963m\DC2\187853D\1092068?5\NAK\167397zIJ\1053903\98929a\1000952\DC1\1002151S8\DC2\ACKB\190196RS\38171\147782\DC1_")}
testObject_DisableLegalHoldForUserRequest_team_5 :: DisableLegalHoldForUserRequest
testObject_DisableLegalHoldForUserRequest_team_5 = DisableLegalHoldForUserRequest {dlhfuPassword = Just (PlainTextPassword "T\50516M\1010309: \53517N\f?z\ACK|Q\1110092]\SOH\39467\v\1077616\r*Z\993106\183296\1092774 \98436\145883W.\1110948\16707\156445\1006764mVD)N'3\140738\1056941\&7\STX")}
