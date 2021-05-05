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
testObject_Team_1 :: Team
testObject_Team_1 = (newTeam ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002")))) ("\128588\32133_R2") ("xg\RSZ\162906") (Binding) & teamIconKey .~ (Just "1"))
testObject_Team_2 :: Team
testObject_Team_2 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000000000002")))) ("") ("S\189572\vi") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_3 :: Team
testObject_Team_3 = (newTeam ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000001")))) ("s\b") ("\160852") (NonBinding) & teamIconKey .~ (Nothing))
testObject_Team_4 :: Team
testObject_Team_4 = (newTeam ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000003")))) ("V\49413}\NAK") ("k\fJZ{") (Binding) & teamIconKey .~ (Nothing))
testObject_Team_5 :: Team
testObject_Team_5 = (newTeam ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000002")))) ("\SIv(") ("\1064591\RS2\NAK") (NonBinding) & teamIconKey .~ (Just "\b\19836\1078010["))
