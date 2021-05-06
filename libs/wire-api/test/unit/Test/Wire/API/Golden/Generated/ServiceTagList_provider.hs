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
testObject_ServiceTagList_provider_1 = ServiceTagList [MedicalTag,MoviesTag,NewsTag,PollTag,SportsTag,HealthTag,HealthTag,EducationTag,RatingTag,NewsTag]
testObject_ServiceTagList_provider_2 :: ServiceTagList
testObject_ServiceTagList_provider_2 = ServiceTagList [FitnessTag,BooksTag,MediaTag,BusinessTag,ShoppingTag,TutorialTag,PollTag,SportsTag,ProductivityTag,MusicTag,MoviesTag,VideoTag,MoviesTag,MusicTag,ProductivityTag,MedicalTag,BusinessTag,EducationTag,BooksTag,TutorialTag,DesignTag,FitnessTag,GraphicsTag,EducationTag]
testObject_ServiceTagList_provider_3 :: ServiceTagList
testObject_ServiceTagList_provider_3 = ServiceTagList [GamesTag,BooksTag,MedicalTag]
testObject_ServiceTagList_provider_4 :: ServiceTagList
testObject_ServiceTagList_provider_4 = ServiceTagList [GamesTag,SportsTag,AudioTag,DesignTag,SocialTag,RatingTag,SocialTag,DesignTag,QuizTag,EducationTag,PhotographyTag,PollTag,FoodDrinkTag,EducationTag,EducationTag,TravelTag,MediaTag,RatingTag]
testObject_ServiceTagList_provider_5 :: ServiceTagList
testObject_ServiceTagList_provider_5 = ServiceTagList [ProductivityTag,EducationTag,IntegrationTag,BooksTag,SocialTag,NewsTag,PollTag,MediaTag,MoviesTag,DesignTag,HealthTag,FoodDrinkTag,SportsTag,DesignTag,SportsTag,HealthTag]
testObject_ServiceTagList_provider_6 :: ServiceTagList
testObject_ServiceTagList_provider_6 = ServiceTagList [FoodDrinkTag,MoviesTag,GraphicsTag,ShoppingTag]
testObject_ServiceTagList_provider_7 :: ServiceTagList
testObject_ServiceTagList_provider_7 = ServiceTagList [TutorialTag,VideoTag,QuizTag,WeatherTag,EntertainmentTag,MoviesTag,FinanceTag,BusinessTag,GraphicsTag,LifestyleTag,VideoTag,MediaTag,BooksTag,AudioTag,BusinessTag,VideoTag,EntertainmentTag,ProductivityTag,SocialTag,FoodDrinkTag,BooksTag,ProductivityTag,HealthTag,EducationTag,WeatherTag]
testObject_ServiceTagList_provider_8 :: ServiceTagList
testObject_ServiceTagList_provider_8 = ServiceTagList [QuizTag,EducationTag,EducationTag,NewsTag,EntertainmentTag]
testObject_ServiceTagList_provider_9 :: ServiceTagList
testObject_ServiceTagList_provider_9 = ServiceTagList [FinanceTag,NewsTag,PhotographyTag,DesignTag,FoodDrinkTag,EntertainmentTag,GamesTag,FinanceTag,EntertainmentTag,DesignTag,DesignTag,SocialTag,TravelTag,IntegrationTag,ShoppingTag,FitnessTag,QuizTag,TutorialTag,SportsTag,MedicalTag,GraphicsTag,HealthTag,MusicTag,BooksTag]
testObject_ServiceTagList_provider_10 :: ServiceTagList
testObject_ServiceTagList_provider_10 = ServiceTagList [TutorialTag,NewsTag,GraphicsTag,BusinessTag,FoodDrinkTag,TutorialTag,ProductivityTag,FoodDrinkTag,ShoppingTag,SportsTag,SocialTag,GraphicsTag,MoviesTag,SportsTag,FinanceTag,BooksTag,PollTag,FoodDrinkTag,BooksTag,EducationTag,ShoppingTag]
testObject_ServiceTagList_provider_11 :: ServiceTagList
testObject_ServiceTagList_provider_11 = ServiceTagList [MediaTag,VideoTag,PollTag,MedicalTag,LifestyleTag,EntertainmentTag,FinanceTag,TravelTag,TravelTag,FoodDrinkTag]
testObject_ServiceTagList_provider_12 :: ServiceTagList
testObject_ServiceTagList_provider_12 = ServiceTagList []
testObject_ServiceTagList_provider_13 :: ServiceTagList
testObject_ServiceTagList_provider_13 = ServiceTagList [AudioTag,WeatherTag,RatingTag,SocialTag,FoodDrinkTag,MoviesTag,ShoppingTag,GraphicsTag,AudioTag,GraphicsTag,RatingTag,IntegrationTag,EducationTag,NewsTag,MusicTag,PollTag,PhotographyTag,MoviesTag,QuizTag,ProductivityTag,SportsTag,WeatherTag,TutorialTag,FoodDrinkTag,BooksTag,PollTag,BusinessTag]
testObject_ServiceTagList_provider_14 :: ServiceTagList
testObject_ServiceTagList_provider_14 = ServiceTagList [WeatherTag,MediaTag,FitnessTag,GraphicsTag,BusinessTag,DesignTag,MusicTag,ProductivityTag,BusinessTag,BooksTag,FitnessTag,AudioTag,PollTag]
testObject_ServiceTagList_provider_15 :: ServiceTagList
testObject_ServiceTagList_provider_15 = ServiceTagList [PollTag,DesignTag,BooksTag,ProductivityTag,QuizTag,FoodDrinkTag,FinanceTag,AudioTag,BusinessTag,LifestyleTag,AudioTag,NewsTag]
testObject_ServiceTagList_provider_16 :: ServiceTagList
testObject_ServiceTagList_provider_16 = ServiceTagList [RatingTag,EntertainmentTag,SportsTag,DesignTag,WeatherTag,SportsTag,SocialTag,TravelTag,PollTag,MusicTag,LifestyleTag,FinanceTag,MedicalTag,NewsTag,ProductivityTag,PhotographyTag,LifestyleTag,LifestyleTag,HealthTag,HealthTag]
testObject_ServiceTagList_provider_17 :: ServiceTagList
testObject_ServiceTagList_provider_17 = ServiceTagList [EducationTag,VideoTag]
testObject_ServiceTagList_provider_18 :: ServiceTagList
testObject_ServiceTagList_provider_18 = ServiceTagList [QuizTag,SocialTag,LifestyleTag]
testObject_ServiceTagList_provider_19 :: ServiceTagList
testObject_ServiceTagList_provider_19 = ServiceTagList [RatingTag,HealthTag,FitnessTag,TravelTag,WeatherTag,WeatherTag,AudioTag,ShoppingTag,MoviesTag,ShoppingTag,SportsTag,PhotographyTag,QuizTag,FitnessTag,MusicTag,ProductivityTag,ShoppingTag]
testObject_ServiceTagList_provider_20 :: ServiceTagList
testObject_ServiceTagList_provider_20 = ServiceTagList [QuizTag,HealthTag,RatingTag,VideoTag,RatingTag,SportsTag]
