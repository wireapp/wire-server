{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.BotUserView_provider where

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
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_BotUserView_provider_1 :: BotUserView
testObject_BotUserView_provider_1 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000800000001"))), botUserViewName = Name {fromName = "w\1111696\78105\SYN=\SUB\173458\1047916\&3\24001qR\191301\1086709\CAN]%8Y`\1035358\ENQ\STX.{q\EM}&\49828\EMS\EM55<$jl)}\GS\1023101bo^\GSZW\\\1026818Q\1107572f%y1\bX\NUL\41417D\EOT\185702c\"v7\1029851\&4S\ACKUXUA#\149540A!\EOT\9646"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Just (Handle {fromHandle = "0.0jnzpwqz1-ftcxd0bx_r1ceasi4gmz4opw7q9brzw5bhdu7j4-ogvb-5q.h22hrwfvn2gk0.xtt4u20bx67r8s6b6qmoczerk7q0kyys6sb09hhe66706u7jrjdpyxj0v8xspvpn1i3xd-mig1l_90fv5-ki329-w.w7vo-a87emk5uvkl"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000800000006")))}
testObject_BotUserView_provider_2 :: BotUserView
testObject_BotUserView_provider_2 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000200000005"))), botUserViewName = Name {fromName = "\22435W\1074113a1;-tCq&\ACKZ}cVW\995521\&6s\118890nD\1073125\"\64684Qf\rmlC_\FS\GS3f\ETX\GSw%\986929|.\162405Z"}, botUserViewColour = ColourId {fromColourId = -2}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0007-0000-000800000007")))}
testObject_BotUserView_provider_3 :: BotUserView
testObject_BotUserView_provider_3 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000100000002"))), botUserViewName = Name {fromName = "\US_\127860^4*\151377Y\SYN\US4G0by\136576%\146006/\97802 \GSP\EOT.:G'\SO\CAN\DC3\1090636\166793e\172953!\1085581$`\1096654\SOHc\"\STX\DLE\STX\1076626\f\ETX_A\1043229\ETB\1063629\ENQ\ESCf\tcg\171125\42396\EM\DLE\48482#i\ETX>\46096IK)\1113587\EOT\CAN\b\99535X\51567hV"}, botUserViewColour = ColourId {fromColourId = 2}, botUserViewHandle = Just (Handle {fromHandle = "bd8jgzzsh7wlup3f_k7-9b50be.niuimqratmugbia9foemfsato4epv39tstc1uhdikjp7d1134f_aa2_sa_p9taxu-oe2d3hnj-3fgqcet7oq3c1x1rik7ypkn-nt6.tn"}), botUserViewTeam = Nothing}
testObject_BotUserView_provider_4 :: BotUserView
testObject_BotUserView_provider_4 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000700000007"))), botUserViewName = Name {fromName = "hA\SOH[`4p7K\128083\1046718-93\144236\bA\34781:-M+lf\bj (m6\f\EM\54959\8810\SUB9\1049082\SOHtM\166063\&0\ESCu['\145628\55077\1036283X1W\DC4s\DEL\63254\&1H\GS\1059159,\99043\DC3F\v\DEL\119201_k\65334 \170467yP"}, botUserViewColour = ColourId {fromColourId = 5}, botUserViewHandle = Nothing, botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000700000007")))}
testObject_BotUserView_provider_5 :: BotUserView
testObject_BotUserView_provider_5 = BotUserView {botUserViewId = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000800000001"))), botUserViewName = Name {fromName = "\1069514}\52342\&2l/\ACKM\SI?J\NUL\DLE\1098774e\RS1_o~\b"}, botUserViewColour = ColourId {fromColourId = 1}, botUserViewHandle = Just (Handle {fromHandle = "e1lkfpocvr.kc5qajmfuc4.843a.xnh3z2dtjk-80n94kwm61f_2693d11ih7iy1l4i9rh8w.trswft9-hgfgh8zwrv89zp630z0047-9kh0bngtopmpm5u97eg80t6iz9iyy3788_tfi0fvyruo98uo0v8q6084doc4-n2q.rm8w09f4795z6-c647eqe04u_"}), botUserViewTeam = Just (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000700000005")))}
