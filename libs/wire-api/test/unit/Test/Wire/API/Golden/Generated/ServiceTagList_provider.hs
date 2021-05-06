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
testObject_ServiceTagList_provider_1 = ServiceTagList [ShoppingTag,ProductivityTag,FinanceTag,AudioTag,BooksTag,RatingTag,WeatherTag,FinanceTag,TutorialTag,SportsTag,NewsTag,GamesTag,RatingTag,NewsTag,FoodDrinkTag,GraphicsTag,SportsTag,AudioTag,BusinessTag,TutorialTag,BooksTag,TutorialTag]
testObject_ServiceTagList_provider_2 :: ServiceTagList
testObject_ServiceTagList_provider_2 = ServiceTagList [FinanceTag,NewsTag,MusicTag,ShoppingTag,BooksTag,SportsTag,MedicalTag,ShoppingTag,HealthTag,FitnessTag,PollTag,LifestyleTag,MediaTag,HealthTag,AudioTag,GraphicsTag,SocialTag,AudioTag,SocialTag,MoviesTag,TutorialTag,QuizTag,WeatherTag,LifestyleTag,RatingTag,QuizTag,MedicalTag,VideoTag,ProductivityTag,NewsTag]
testObject_ServiceTagList_provider_3 :: ServiceTagList
testObject_ServiceTagList_provider_3 = ServiceTagList [ProductivityTag,AudioTag,DesignTag,TravelTag,FoodDrinkTag,SocialTag,VideoTag,WeatherTag,SocialTag,FinanceTag,WeatherTag,GraphicsTag,BooksTag,LifestyleTag,ProductivityTag,MedicalTag,RatingTag,DesignTag,NewsTag,GamesTag,DesignTag,MoviesTag,PhotographyTag,WeatherTag,ShoppingTag,MediaTag]
testObject_ServiceTagList_provider_4 :: ServiceTagList
testObject_ServiceTagList_provider_4 = ServiceTagList [VideoTag,ShoppingTag,FitnessTag,MediaTag,EducationTag,BusinessTag,QuizTag,ProductivityTag,SocialTag,QuizTag,QuizTag,MediaTag,PollTag,BooksTag,MoviesTag,FoodDrinkTag,EducationTag,SportsTag,MoviesTag,TravelTag,RatingTag,IntegrationTag]
testObject_ServiceTagList_provider_5 :: ServiceTagList
testObject_ServiceTagList_provider_5 = ServiceTagList [TutorialTag,MusicTag]
testObject_ServiceTagList_provider_6 :: ServiceTagList
testObject_ServiceTagList_provider_6 = ServiceTagList [NewsTag,FoodDrinkTag,LifestyleTag,BusinessTag]
testObject_ServiceTagList_provider_7 :: ServiceTagList
testObject_ServiceTagList_provider_7 = ServiceTagList [ProductivityTag,MusicTag,EducationTag,MediaTag,LifestyleTag,IntegrationTag,VideoTag,ProductivityTag,PollTag,ShoppingTag,MoviesTag,TravelTag]
testObject_ServiceTagList_provider_8 :: ServiceTagList
testObject_ServiceTagList_provider_8 = ServiceTagList [MusicTag,DesignTag,RatingTag,LifestyleTag,MoviesTag,GamesTag,NewsTag,PhotographyTag,WeatherTag]
testObject_ServiceTagList_provider_9 :: ServiceTagList
testObject_ServiceTagList_provider_9 = ServiceTagList [PollTag,MoviesTag,FitnessTag,MoviesTag,GamesTag,PollTag,FitnessTag,HealthTag,EntertainmentTag,SocialTag,SportsTag]
testObject_ServiceTagList_provider_10 :: ServiceTagList
testObject_ServiceTagList_provider_10 = ServiceTagList [PhotographyTag,SocialTag,SportsTag,TutorialTag,EntertainmentTag,GamesTag,IntegrationTag]
testObject_ServiceTagList_provider_11 :: ServiceTagList
testObject_ServiceTagList_provider_11 = ServiceTagList [EntertainmentTag,VideoTag,PollTag,VideoTag,SocialTag,GamesTag,AudioTag,HealthTag,AudioTag,BooksTag,FoodDrinkTag,GraphicsTag,RatingTag,MediaTag,SocialTag,FoodDrinkTag,PollTag,TutorialTag,FoodDrinkTag,TutorialTag,SocialTag,GamesTag,VideoTag,GamesTag,FitnessTag,RatingTag,WeatherTag]
testObject_ServiceTagList_provider_12 :: ServiceTagList
testObject_ServiceTagList_provider_12 = ServiceTagList [NewsTag,SportsTag,ProductivityTag,FinanceTag,MoviesTag,DesignTag,DesignTag,NewsTag,EducationTag,AudioTag,FinanceTag,GraphicsTag,QuizTag,BooksTag,ProductivityTag,BusinessTag,RatingTag,AudioTag,AudioTag,TutorialTag,MediaTag,FinanceTag,FinanceTag,GamesTag,TutorialTag,TutorialTag]
testObject_ServiceTagList_provider_13 :: ServiceTagList
testObject_ServiceTagList_provider_13 = ServiceTagList [EducationTag,GraphicsTag,MedicalTag,IntegrationTag,BusinessTag,AudioTag,EducationTag,PhotographyTag,ProductivityTag,GraphicsTag,MediaTag,HealthTag,MedicalTag,FinanceTag,QuizTag,TutorialTag,MedicalTag,WeatherTag,TravelTag,FitnessTag]
testObject_ServiceTagList_provider_14 :: ServiceTagList
testObject_ServiceTagList_provider_14 = ServiceTagList [TravelTag,WeatherTag]
testObject_ServiceTagList_provider_15 :: ServiceTagList
testObject_ServiceTagList_provider_15 = ServiceTagList [GraphicsTag,MedicalTag,EntertainmentTag,TutorialTag,ProductivityTag,MoviesTag,ProductivityTag,MusicTag,MedicalTag,PhotographyTag,AudioTag,PollTag,PollTag,SportsTag,AudioTag,MediaTag,FitnessTag,TravelTag,MediaTag,MoviesTag,EntertainmentTag,MedicalTag,ShoppingTag,MediaTag,ProductivityTag,FitnessTag,QuizTag,FoodDrinkTag]
testObject_ServiceTagList_provider_16 :: ServiceTagList
testObject_ServiceTagList_provider_16 = ServiceTagList [GraphicsTag,PollTag,RatingTag,AudioTag,DesignTag,TravelTag,IntegrationTag,ProductivityTag,WeatherTag,GraphicsTag,MoviesTag,MedicalTag,FinanceTag,MusicTag,GamesTag,IntegrationTag,LifestyleTag,DesignTag,MediaTag,QuizTag,ShoppingTag,PhotographyTag]
testObject_ServiceTagList_provider_17 :: ServiceTagList
testObject_ServiceTagList_provider_17 = ServiceTagList [DesignTag,MusicTag,TutorialTag]
testObject_ServiceTagList_provider_18 :: ServiceTagList
testObject_ServiceTagList_provider_18 = ServiceTagList [VideoTag,IntegrationTag,SocialTag,MediaTag,ProductivityTag,LifestyleTag,ProductivityTag]
testObject_ServiceTagList_provider_19 :: ServiceTagList
testObject_ServiceTagList_provider_19 = ServiceTagList [RatingTag,BusinessTag,MedicalTag,GraphicsTag,PollTag,FitnessTag]
testObject_ServiceTagList_provider_20 :: ServiceTagList
testObject_ServiceTagList_provider_20 = ServiceTagList [MoviesTag,NewsTag,SocialTag,EntertainmentTag,RatingTag,GamesTag,BusinessTag,PhotographyTag,MusicTag,BooksTag,RatingTag,TravelTag,PhotographyTag,HealthTag,LifestyleTag,VideoTag,FoodDrinkTag,PollTag,EducationTag,ShoppingTag,FinanceTag,DesignTag,TutorialTag,GamesTag,EntertainmentTag,TravelTag,TutorialTag,EducationTag,LifestyleTag]
