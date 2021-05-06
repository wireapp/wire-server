{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Event_team where

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
testObject_Event_team_1 :: Event
testObject_Event_team_1 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00001918-0000-4e4b-0000-1404000051c0")))) (read ("1864-05-27 07:26:11.655020508387 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000001")))) ("") ("x>\1041317\35771") (Binding) & teamIconKey .~ (Just "PY")))))
testObject_Event_team_2 :: Event
testObject_Event_team_2 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "000054f7-0000-2fbd-0000-696100005f95")))) (read ("1864-04-15 11:46:59.003122210728 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000003")))) ("2\134759") ("R|cq%") (NonBinding) & teamIconKey .~ (Just "aM")))))
testObject_Event_team_3 :: Event
testObject_Event_team_3 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00004b10-0000-4423-0000-194c00002cb6")))) (read ("1864-05-11 09:33:50.456440466259 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "000054af-0000-21c1-0000-3eb7000048f0"))))))
testObject_Event_team_4 :: Event
testObject_Event_team_4 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00001fc8-0000-1bdb-0000-45f000007398")))) (read ("1864-04-24 17:54:56.053859630505 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "00007438-0000-75f0-0000-3b3d0000024f"))))))
testObject_Event_team_5 :: Event
testObject_Event_team_5 = (newEvent (TeamDelete) ((Id (fromJust (UUID.fromString "00001de7-0000-42e1-0000-74d9000074b8")))) (read ("1864-05-28 09:40:52.568638116666 UTC")) & eventData .~ (Nothing))
testObject_Event_team_6 :: Event
testObject_Event_team_6 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00007c91-0000-4394-0000-088a00007523")))) (read ("1864-06-03 22:19:06.653469695753 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000002")))) ("6\a") ("\SYN\t.d") (NonBinding) & teamIconKey .~ (Just "\DC1\1007944\20643\111098")))))
testObject_Event_team_7 :: Event
testObject_Event_team_7 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "00002b82-0000-03b5-0000-24ed00001e20")))) (read ("1864-05-12 22:23:24.065569155369 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("Ky\r\v~\12209T\ACK\165253?$\\s\45972\&5^\DC4\1013071\t5")), _iconUpdate = Just (unsafeRange ("wq|]cLP9\DC3{>.\1088066\USN,\172627\t\FS\STX\1032055UI\183223\&1\SI\DLEwF\40190d{\40808D\12550\ENQ\DELe\48921\134078\DC1:v(n\ESC\21295!jj\SI\GS\20821#\a~\168308g\ESC\178760\1103261;\ETX*\191439\f ~cV\1051251]k\4221>ft;`\ACKV\DC38\1102460\172049\188621")), _iconKeyUpdate = Just (unsafeRange ("\1061225r~\DEL\1014671TY&s sh\r\1004860JxRelhY\EOT=<6U<\995160aA_3\149913\&5\DC4\RS\DC4\NAK?v\1003791i"))}))))
testObject_Event_team_8 :: Event
testObject_Event_team_8 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "00004141-0000-4f90-0000-1d9600004f8c")))) (read ("1864-04-19 23:49:07.81068597114 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "00006604-0000-42d4-0000-6924000041a7"))))))
testObject_Event_team_9 :: Event
testObject_Event_team_9 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "000031e5-0000-4dfa-0000-416d00004318")))) (read ("1864-05-02 23:03:51.768011919542 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "0000398a-0000-30d2-0000-5b34000018fe"))))))
testObject_Event_team_10 :: Event
testObject_Event_team_10 = (newEvent (MemberLeave) ((Id (fromJust (UUID.fromString "0000077c-0000-3fcc-0000-1678000010c3")))) (read ("1864-05-17 12:43:34.601411311421 UTC")) & eventData .~ (Just (EdMemberLeave (Id (fromJust (UUID.fromString "00005307-0000-7bea-0000-6fd4000055c9"))))))
testObject_Event_team_11 :: Event
testObject_Event_team_11 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "000010b1-0000-1f16-0000-580d00004998")))) (read ("1864-05-29 12:37:04.414889721396 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "00004187-0000-6f87-0000-1f6a000067d7"))))))
testObject_Event_team_12 :: Event
testObject_Event_team_12 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "00005761-0000-45c7-0000-1f9800003ee7")))) (read ("1864-05-09 18:23:03.785886406141 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "00003d9e-0000-3c42-0000-4fdc000027b5"))))))
testObject_Event_team_13 :: Event
testObject_Event_team_13 = (newEvent (MemberLeave) ((Id (fromJust (UUID.fromString "000072d3-0000-3f65-0000-28e700003d7a")))) (read ("1864-04-20 00:47:01.590058739072 UTC")) & eventData .~ (Just (EdMemberLeave (Id (fromJust (UUID.fromString "00007ff2-0000-670a-0000-6f2700006df4"))))))
testObject_Event_team_14 :: Event
testObject_Event_team_14 = (newEvent (MemberLeave) ((Id (fromJust (UUID.fromString "000073a8-0000-18de-0000-3c17000011bc")))) (read ("1864-04-09 06:27:41.011961516407 UTC")) & eventData .~ (Just (EdMemberLeave (Id (fromJust (UUID.fromString "000012e2-0000-5c10-0000-299a0000662f"))))))
testObject_Event_team_15 :: Event
testObject_Event_team_15 = (newEvent (ConvCreate) ((Id (fromJust (UUID.fromString "000062ca-0000-411b-0000-552e00006eb2")))) (read ("1864-05-22 14:09:30.807764743585 UTC")) & eventData .~ (Just (EdConvCreate (Id (fromJust (UUID.fromString "00000ca2-0000-7efb-0000-434400001f40"))))))
testObject_Event_team_16 :: Event
testObject_Event_team_16 = (newEvent (ConvDelete) ((Id (fromJust (UUID.fromString "000046f0-0000-5d08-0000-465c00007689")))) (read ("1864-05-08 01:22:21.607822635073 UTC")) & eventData .~ (Just (EdConvDelete (Id (fromJust (UUID.fromString "00001d94-0000-45fe-0000-314000000610"))))))
testObject_Event_team_17 :: Event
testObject_Event_team_17 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "00003ee8-0000-58aa-0000-4d9500003b35")))) (read ("1864-04-16 16:51:15.684518577545 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000000")))) ("\FS6#") ("") (NonBinding) & teamIconKey .~ (Nothing)))))
testObject_Event_team_18 :: Event
testObject_Event_team_18 = (newEvent (TeamUpdate) ((Id (fromJust (UUID.fromString "000077bf-0000-1027-0000-28b400006053")))) (read ("1864-04-13 21:28:41.478465237189 UTC")) & eventData .~ (Just (EdTeamUpdate (TeamUpdateData {_nameUpdate = Just (unsafeRange ("\14357j\27428\1002108G\1096746JFD7\194902\1016820b\FS\FSI&\EOTG\EOT\SYN*S4Jtj\ACK\13971\DEL%v<fKJ\44984\28386^\\fY\140086&ve\CAN\1025778A\"s\1089146n\143623x\DELy\NUL\ETB\NAKq(}\1010736`Q\DLE\137932(8Rrf\EM\CAN\CAN7d\STXV\1030958lh\1058986;\DC3q\18165\CAN\121117\STX\171566\FS;vK\21930$\1026236\1051593wuGd\136976c*JqlTlh\\\FS\1065314y7=\SOH\DC2\1066445cL\ETBq\STX].rT!dW\tPe\1088211")), _iconUpdate = Just (unsafeRange (">'W\SI}7*B\121199\143053\37811\ETXpqB\1018681HV\83385`\1102265\1006684a`&\EM\1042884\1107786PI>\1101003\1073068u7;\EMc/b\189325\1053951\EOT]r@\1019627[\10623\ACK3*v\CAN\NAK.7\EM\1061569\DC1\62889\&6\1010431\191024\985039\n\FS\ACK>*o6\39290V{4\ACK(\1012743q\1000557h1(\SI,m:z\1078718$\SIgZ\37532\99136\&3mQJmn\30934(\14549n\5647D\987393\1039689\&3\989783\ETB\1103723z\DC3tA*\148285\9523\rQ_{ln\ETBxc\147797vx\1024417w /tm]\NAK&B\58929F7\NAK6P Z0u#\ACK\111162\37992\DC3\US\985825\156886\DC2L\52982::G<\r\tr\984690bVG\83107m\44001: w&A\SO(\SYN%2~\GS\NUL\92382A-\154177\n:`\1062487\178855ZB\1079118pi\US>\CAN\1030951s\985426$\177146\bE\ETB\142034x`A;\DC2")), _iconKeyUpdate = Nothing}))))
testObject_Event_team_19 :: Event
testObject_Event_team_19 = (newEvent (MemberJoin) ((Id (fromJust (UUID.fromString "00000b36-0000-2c1b-0000-0b5c000039d3")))) (read ("1864-04-28 14:40:24.548087983945 UTC")) & eventData .~ (Just (EdMemberJoin (Id (fromJust (UUID.fromString "000027f0-0000-2bff-0000-482b000038f7"))))))
testObject_Event_team_20 :: Event
testObject_Event_team_20 = (newEvent (TeamCreate) ((Id (fromJust (UUID.fromString "0000641e-0000-593e-0000-355f00003818")))) (read ("1864-05-11 15:33:37.798547773039 UTC")) & eventData .~ (Just (EdTeamCreate (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000400000001")))) ("ITp1") ("\34164\1070264\2965\EM\83077") (Binding) & teamIconKey .~ (Just ".M\11320\DC3")))))
