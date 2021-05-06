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
testObject_Team_team_1 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000001")))) ("\47359\&4\24473\162522\DLE") ("i,_O\133563") (Binding) & teamIconKey .~ (Just "&["))
testObject_Team_team_2 :: Team
testObject_Team_team_2 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000000")))) ("\DC4") ("\26556\149399\1048457\ETB\1074842") (NonBinding) & teamIconKey .~ (Just "\a\1100190Q"))
testObject_Team_team_3 :: Team
testObject_Team_team_3 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000002")))) ("\DC2") ("\1101838h\994056") (NonBinding) & teamIconKey .~ (Just ""))
testObject_Team_team_4 :: Team
testObject_Team_team_4 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000000")))) ("a\b\GSN") ("\179228\178271E\57805") (NonBinding) & teamIconKey .~ (Just "\1054419M\1054608;U"))
testObject_Team_team_5 :: Team
testObject_Team_team_5 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000400000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000002")))) ("F\184102R\57872z") ("}\33976\1056950\1067190b") (NonBinding) & teamIconKey .~ (Just "\4639\DC2{\128268h"))
testObject_Team_team_6 :: Team
testObject_Team_team_6 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000200000003")))) ("\989301\&4\NUL") ("\tE") (Binding) & teamIconKey .~ (Just ""))
testObject_Team_team_7 :: Team
testObject_Team_team_7 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000003")))) ("\STXc") ("}") (NonBinding) & teamIconKey .~ (Just "\FS\1014038"))
testObject_Team_team_8 :: Team
testObject_Team_team_8 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000001")))) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000300000000")))) (")\DLE0\DLE)") ("\CANO\1099279;") (NonBinding) & teamIconKey .~ (Just "\46517"))
testObject_Team_team_9 :: Team
testObject_Team_team_9 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000")))) ("") ("\49960A\1079630") (NonBinding) & teamIconKey .~ (Just "\ETB"))
testObject_Team_team_10 :: Team
testObject_Team_team_10 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000004")))) (")\EM\1052579\155765") ("\SO%g") (NonBinding) & teamIconKey .~ (Just "\186916J'"))
testObject_Team_team_11 :: Team
testObject_Team_team_11 = (newTeam ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000200000003")))) ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000000000000")))) ("u\ACK>=\1059092") ("") (NonBinding) & teamIconKey .~ (Just "\ENQ\1088355~\1001969\&6"))
testObject_Team_team_12 :: Team
testObject_Team_team_12 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000004")))) ("\v:\1091627") ("\DC3un;h") (Binding) & teamIconKey .~ (Just "O"))
testObject_Team_team_13 :: Team
testObject_Team_team_13 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000003")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000000")))) ("\1014005\r") ("\18790\DC29bi") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_14 :: Team
testObject_Team_team_14 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000")))) ("") ("\DC26\1078977z") (Binding) & teamIconKey .~ (Just "\DC2\fq"))
testObject_Team_team_15 :: Team
testObject_Team_team_15 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000400000002")))) ("j") ("\ESC~") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_16 :: Team
testObject_Team_team_16 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000004")))) ("4N+;") ("o") (NonBinding) & teamIconKey .~ (Just ""))
testObject_Team_team_17 :: Team
testObject_Team_team_17 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000001")))) ("\34790\ACK\178568\ACKw") ("") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_18 :: Team
testObject_Team_team_18 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000004")))) ("BW\167652\9852") ("\1007077\SUBv") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_19 :: Team
testObject_Team_team_19 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000000")))) ("?\194602e") ("") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_team_20 :: Team
testObject_Team_team_20 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000003")))) ("\EM9&\SO ") ("6VZ") (NonBinding) & teamIconKey .~ (Just "5="))
