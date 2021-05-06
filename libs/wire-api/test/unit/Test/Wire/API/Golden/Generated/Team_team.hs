{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Team_team where

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
testObject_Team_team_1 :: Team
testObject_Team_team_1 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000400000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000300000001")))) ("F5\998985\136641") ("1'k") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_2 :: Team
testObject_Team_team_2 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000004")))) ("\DC4w o") ("e") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_3 :: Team
testObject_Team_team_3 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000003")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000003")))) ("\SYN") ("\a%`") (Binding) & teamIconKey .~ (Just "FV"))
testObject_Team_team_4 :: Team
testObject_Team_team_4 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000000")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000004")))) ("\t#_@\DEL") ("\SYNn\DC4") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_5 :: Team
testObject_Team_team_5 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000003")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000001")))) ("\f/v") ("u42\1005633") (NonBinding) & teamIconKey .~ (Just "\993191_9"))
testObject_Team_team_6 :: Team
testObject_Team_team_6 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000002")))) ("OZ") ("3|\1046780\139877") (Binding) & teamIconKey .~ (Just "\1052699q\EOT\1008846"))
testObject_Team_team_7 :: Team
testObject_Team_team_7 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000002")))) ("A") ("Gc\ACK\1010546") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_8 :: Team
testObject_Team_team_8 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001")))) ("t\101045") ("X\1107400") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_9 :: Team
testObject_Team_team_9 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000004")))) ("%") ("\10509ry") (Binding) & teamIconKey .~ (Just "^\ENQ\1095313"))
testObject_Team_team_10 :: Team
testObject_Team_team_10 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))) ("\1057436\DC3\RSC") ("\a\151811") (NonBinding) & teamIconKey .~ (Just ""))
testObject_Team_team_11 :: Team
testObject_Team_team_11 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000100000003")))) ("\SYN5\997734\SO") ("\1025853|") (NonBinding) & teamIconKey .~ (Just "@\1045907y\190771"))
testObject_Team_team_12 :: Team
testObject_Team_team_12 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000003")))) ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000001")))) ("\1071691\32598\US") ("\DC3\68016\1003315!N") (NonBinding) & teamIconKey .~ (Just "\n6"))
testObject_Team_team_13 :: Team
testObject_Team_team_13 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000")))) ("\RS\CAN%\DC1") (":o\60106`") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_14 :: Team
testObject_Team_team_14 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002")))) ("\ETB+") ("Z]") (Binding) & teamIconKey .~ (Just "\1024591"))
testObject_Team_team_15 :: Team
testObject_Team_team_15 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000300000004")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000001")))) ("\r@\95222<v") ("Dh#V\1048366") (Binding) & teamIconKey .~ (Just ""))
testObject_Team_team_16 :: Team
testObject_Team_team_16 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000004")))) ("W_\EMj\1098300") ("") (NonBinding) & teamIconKey .~ (Just " \118944s`"))
testObject_Team_team_17 :: Team
testObject_Team_team_17 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000004")))) ("/") ("") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_18 :: Team
testObject_Team_team_18 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000003")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000003")))) ("p\SUB\1055376") ("\\\163507") (NonBinding) & teamIconKey .~ (Just "\1008447qi\1013086\1053702"))
testObject_Team_team_19 :: Team
testObject_Team_team_19 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000001")))) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000000")))) ("\DC21\135617\57449") ("") (Binding) & teamIconKey .~ (Just "R\b\151740z"))
testObject_Team_team_20 :: Team
testObject_Team_team_20 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001")))) ("B,\r") ("\b") (Binding) & teamIconKey .~ (Just "`\ETB\18764l"))
