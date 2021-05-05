{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamConversation_team where

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
testObject_TeamConversation_1 :: TeamConversation
testObject_TeamConversation_1 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000042-0000-0076-0000-006a00000045")))) (True))
testObject_TeamConversation_2 :: TeamConversation
testObject_TeamConversation_2 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000065-0000-001e-0000-007500000021")))) (True))
testObject_TeamConversation_3 :: TeamConversation
testObject_TeamConversation_3 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000055-0000-0073-0000-00030000006c")))) (True))
testObject_TeamConversation_4 :: TeamConversation
testObject_TeamConversation_4 = (newTeamConversation ((Id (fromJust (UUID.fromString "0000001e-0000-0012-0000-00080000005e")))) (False))
testObject_TeamConversation_5 :: TeamConversation
testObject_TeamConversation_5 = (newTeamConversation ((Id (fromJust (UUID.fromString "00000044-0000-0044-0000-003d0000003b")))) (False))
