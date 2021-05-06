{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ServiceTag_provider where

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
testObject_ServiceTag_provider_1 :: ServiceTag
testObject_ServiceTag_provider_1 = LifestyleTag
testObject_ServiceTag_provider_2 :: ServiceTag
testObject_ServiceTag_provider_2 = HealthTag
testObject_ServiceTag_provider_3 :: ServiceTag
testObject_ServiceTag_provider_3 = VideoTag
testObject_ServiceTag_provider_4 :: ServiceTag
testObject_ServiceTag_provider_4 = MoviesTag
testObject_ServiceTag_provider_5 :: ServiceTag
testObject_ServiceTag_provider_5 = MedicalTag
testObject_ServiceTag_provider_6 :: ServiceTag
testObject_ServiceTag_provider_6 = FinanceTag
testObject_ServiceTag_provider_7 :: ServiceTag
testObject_ServiceTag_provider_7 = PollTag
testObject_ServiceTag_provider_8 :: ServiceTag
testObject_ServiceTag_provider_8 = SportsTag
testObject_ServiceTag_provider_9 :: ServiceTag
testObject_ServiceTag_provider_9 = FinanceTag
testObject_ServiceTag_provider_10 :: ServiceTag
testObject_ServiceTag_provider_10 = SocialTag
testObject_ServiceTag_provider_11 :: ServiceTag
testObject_ServiceTag_provider_11 = DesignTag
testObject_ServiceTag_provider_12 :: ServiceTag
testObject_ServiceTag_provider_12 = ShoppingTag
testObject_ServiceTag_provider_13 :: ServiceTag
testObject_ServiceTag_provider_13 = QuizTag
testObject_ServiceTag_provider_14 :: ServiceTag
testObject_ServiceTag_provider_14 = EntertainmentTag
testObject_ServiceTag_provider_15 :: ServiceTag
testObject_ServiceTag_provider_15 = SocialTag
testObject_ServiceTag_provider_16 :: ServiceTag
testObject_ServiceTag_provider_16 = GraphicsTag
testObject_ServiceTag_provider_17 :: ServiceTag
testObject_ServiceTag_provider_17 = SportsTag
testObject_ServiceTag_provider_18 :: ServiceTag
testObject_ServiceTag_provider_18 = EducationTag
testObject_ServiceTag_provider_19 :: ServiceTag
testObject_ServiceTag_provider_19 = VideoTag
testObject_ServiceTag_provider_20 :: ServiceTag
testObject_ServiceTag_provider_20 = AudioTag
