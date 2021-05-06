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
testObject_Team_team_1 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000004")))) ("") ("mx") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_2 :: Team
testObject_Team_team_2 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000004")))) ("J-q") (",L\1068211") (NonBinding) & teamIconKey .~ (Just "\121041"))
testObject_Team_team_3 :: Team
testObject_Team_team_3 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000004")))) ("") ("mL\1091134\DC4\STX") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_team_4 :: Team
testObject_Team_team_4 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001")))) ("\NUL") (")\EOT") (Binding) & teamIconKey .~ (Just "M\1073817"))
testObject_Team_team_5 :: Team
testObject_Team_team_5 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002")))) ("") ("~\US.") (NonBinding) & teamIconKey .~ (Just "\DC1q\42828."))
