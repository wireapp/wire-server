{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamBinding_team where

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
testObject_TeamBinding_team_1 :: TeamBinding
testObject_TeamBinding_team_1 = NonBinding
testObject_TeamBinding_team_2 :: TeamBinding
testObject_TeamBinding_team_2 = NonBinding
testObject_TeamBinding_team_3 :: TeamBinding
testObject_TeamBinding_team_3 = Binding
testObject_TeamBinding_team_4 :: TeamBinding
testObject_TeamBinding_team_4 = Binding
testObject_TeamBinding_team_5 :: TeamBinding
testObject_TeamBinding_team_5 = Binding
testObject_TeamBinding_team_6 :: TeamBinding
testObject_TeamBinding_team_6 = NonBinding
testObject_TeamBinding_team_7 :: TeamBinding
testObject_TeamBinding_team_7 = NonBinding
testObject_TeamBinding_team_8 :: TeamBinding
testObject_TeamBinding_team_8 = NonBinding
testObject_TeamBinding_team_9 :: TeamBinding
testObject_TeamBinding_team_9 = Binding
testObject_TeamBinding_team_10 :: TeamBinding
testObject_TeamBinding_team_10 = NonBinding
testObject_TeamBinding_team_11 :: TeamBinding
testObject_TeamBinding_team_11 = Binding
testObject_TeamBinding_team_12 :: TeamBinding
testObject_TeamBinding_team_12 = Binding
testObject_TeamBinding_team_13 :: TeamBinding
testObject_TeamBinding_team_13 = NonBinding
testObject_TeamBinding_team_14 :: TeamBinding
testObject_TeamBinding_team_14 = Binding
testObject_TeamBinding_team_15 :: TeamBinding
testObject_TeamBinding_team_15 = Binding
testObject_TeamBinding_team_16 :: TeamBinding
testObject_TeamBinding_team_16 = Binding
testObject_TeamBinding_team_17 :: TeamBinding
testObject_TeamBinding_team_17 = Binding
testObject_TeamBinding_team_18 :: TeamBinding
testObject_TeamBinding_team_18 = NonBinding
testObject_TeamBinding_team_19 :: TeamBinding
testObject_TeamBinding_team_19 = Binding
testObject_TeamBinding_team_20 :: TeamBinding
testObject_TeamBinding_team_20 = Binding
