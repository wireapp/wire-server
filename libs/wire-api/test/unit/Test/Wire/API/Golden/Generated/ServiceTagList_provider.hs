{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ServiceTagList_provider where

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
testObject_ServiceTagList_provider_1 :: ServiceTagList
testObject_ServiceTagList_provider_1 = ServiceTagList [MusicTag,PollTag]
testObject_ServiceTagList_provider_2 :: ServiceTagList
testObject_ServiceTagList_provider_2 = ServiceTagList [VideoTag,GamesTag,NewsTag,HealthTag,EducationTag,MedicalTag,WeatherTag,LifestyleTag]
testObject_ServiceTagList_provider_3 :: ServiceTagList
testObject_ServiceTagList_provider_3 = ServiceTagList [DesignTag,TutorialTag,MedicalTag,RatingTag,SportsTag,IntegrationTag,PollTag,RatingTag,QuizTag,DesignTag,DesignTag,EducationTag,EducationTag,QuizTag,AudioTag,BusinessTag,NewsTag,NewsTag,RatingTag,HealthTag,MediaTag,MoviesTag,SportsTag,EntertainmentTag,RatingTag]
testObject_ServiceTagList_provider_4 :: ServiceTagList
testObject_ServiceTagList_provider_4 = ServiceTagList [TutorialTag,MedicalTag,ShoppingTag,GraphicsTag]
testObject_ServiceTagList_provider_5 :: ServiceTagList
testObject_ServiceTagList_provider_5 = ServiceTagList [MediaTag,QuizTag,TravelTag,PollTag,QuizTag,EntertainmentTag,ShoppingTag,HealthTag,TutorialTag,RatingTag,QuizTag,GamesTag,SocialTag,VideoTag,RatingTag,MoviesTag]
