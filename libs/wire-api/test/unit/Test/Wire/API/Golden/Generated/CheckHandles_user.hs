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
testObject_CheckHandles_user_1 = CheckHandles {checkHandlesList = (unsafeRange (["\ETX\163027\EOT\1048681l\1093920\1095180\NAK\137234]","o\43406\DC3&I<\1057781k\1033691\997043\9352","?\984425\124982\1018009\1087155\SOH3","\150356dK\10965\ETX\183292\1025169qJB","\24265\1002136\30441yl\ESC","\ETB9X{\163507,\DC15&\SUB\12413\53859","\ETXI\DELKL\RS\50493Y\DC2y@\119018","19\1030487\1112400\160309\SI\"G","\nO`=\RS<\983266n;\1017340\486",";","y35\150880/\182458Y\DEL\EOT\STX\163693","8p#","\STXK]\1066627\1096742.\985936-\1041968\FS1$","\21312\US\n\57546\153528\51818\CAN>\r9\1045776","\1079117o(b\100853"])), checkHandlesNum = (unsafeRange (10))}
testObject_CheckHandles_user_2 :: CheckHandles
testObject_CheckHandles_user_2 = CheckHandles {checkHandlesList = (unsafeRange (["'x'\40292\990834\&30\136456\b\1268\SUB\175998","V%bD","1\SI\STX\CAN","8","","I\a\SYN\ENQ","\36752\NULM\atfr^\146719","\SI`\SO\a\f","JD\NUL\RSbv\134269>\DELl\RSQ\SI\96186x","y\NUL\EMd)^~\74414q9A5","\175393\ESC+8%\1003081\EM\1078265(","","P[Re,","HK\DC25yT\25633GDT\NUL\FS","\187213\&8#c1","\ftEfe6\8343\1068741\&4xD","","h+\1027024p\1112370\997409","\7349M'^2q^","\1037089L@Gf\1091576'%:","b5\STX6\992650)\1049507\DC1","\161222\990623\&6,,;<\ENQ\1084350o\49361\161729","\94876n\1082365\ETB","\22097","7L","Jq6:","\1101202~","8<\ENQ\EOT:","\1000443\1035214AKEJA\NAK\168039*us\DC2\STX~","\SUBQ\1021565>","",")^x>*\10099","M<\184533\1028036\ENQ"])), checkHandlesNum = (unsafeRange (9))}
testObject_CheckHandles_user_3 :: CheckHandles
testObject_CheckHandles_user_3 = CheckHandles {checkHandlesList = (unsafeRange (["\127744*\62862\num\176367l^r\132848X","\1038048\1044371\1001861+xRS\1047185g\tJ\GS\143098\1060749","\ACK","","\1004733bJqU`","C\1083655+\1006590@\187867\ESC#\1109636z\990982","F\1068957\183737\1060548\1062570","@x9","\1037493\DLE","\1003729\&0a\1002962pD\DC3\40274\DC2L\1035469 \152167\SI","\DC1","\41464\1034109\50359\FS'","U\rv\r\FS(s","\SIKOw%\DC1D*V|","\1053020,oF","\1049589F+2T\1072736\3081^\175620C<2\1082381_","\996464U\17200ZUy\SUB\FS7\121123\63334c\NUL2","K\1090595C\EOTQ}4i.]d","-\4179","@r\1034042\f[\DC4d","\60391t#",",\1081949\996014-\ENQT\1087330\57700\989225","B","{#\1032286R\1085502","\43993\DC3q\1009362\1082058I","`\1035978b\to3{\f","","]F\SIE^r\1063832\989116\ENQQ\1068722\166327kY","d","\9098i,\DEL\ESCi-\997843\47475\t\1092691\986394Rb","Q#\157648\SI\1010756V","[F\RS\ETB\61504","&\ETX\32499\1051856\fXj\166983I^m\DLE","l\FS\ETX\18703%\aymQ\2637\1096224\186968","\1051290\4309\ENQb,%\DC3\NAK;\1014355c\US\US?","\"j)_\1088582\189572\CAN\NUL~\CAN\CAN\DC4","b\SUB8,\123180\993195h","?\GS^}\1023490Zp@$.V\94279\985108q","\32821\25743\&1@\DC2\1085311\175348\ETXS\1047744n\DC2","\f\ETX\ETB\1041696Nt_\71338","7\ESC\ETX\"D\1040421","=","\\\\:.\167426\&1\1017980j*o\156788u\32411\1100612\SOH","\2432\1010830\&6\ETX\156527\&0"," \SOH_U~\158054X~c","\a\STX\DC2q\3205\60565E\5489G:"])), checkHandlesNum = (unsafeRange (5))}
testObject_CheckHandles_user_4 :: CheckHandles
testObject_CheckHandles_user_4 = CheckHandles {checkHandlesList = (unsafeRange (["\1073487","\31066\64896e\32467*","\b","\RS","8\188882","! \148656?\1082718H\tx\FS\1006774Ho\EOT:"])), checkHandlesNum = (unsafeRange (2))}
testObject_CheckHandles_user_5 :: CheckHandles
testObject_CheckHandles_user_5 = CheckHandles {checkHandlesList = (unsafeRange (["","1\1000342\EM;\STX'\132631","","\32154l8m\1031294T:F\STXE","\13780\1113284 \1008708l\RS\176855T\GS6\47744\153371>","","\DC1`4k<\1108526\SIMbB\CAN\119354\v","\NULpF\CAN\66484`\FSL.\146957)\1079180","PT#(kE\EMoN>","\1060968W\FS08%F\STX@`","u\137189","u1\fm\SO_\61421\&0\1103917\DC4\172017\110986\1033589\1022024","Sx-\v","\24146\148399\SO\1037187\SUB\1063236H\DELc#\71476(i5\65187","7\995176\32723c\1018497","","6O\\woE7+\1034596\SI.\188914\f\1070914F","YF>\992068\1018230&N\SIs","\th/\1003695\151523\b\\=\2346\64136"])), checkHandlesNum = (unsafeRange (9))}
