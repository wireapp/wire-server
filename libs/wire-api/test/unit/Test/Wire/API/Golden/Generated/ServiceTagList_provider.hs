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
testObject_ServiceTagList_provider_1 = ServiceTagList [BooksTag,ProductivityTag,SocialTag,FitnessTag,MoviesTag,QuizTag,BooksTag,AudioTag,MediaTag,TutorialTag,ShoppingTag,DesignTag,EntertainmentTag,FinanceTag,PollTag,QuizTag,TravelTag,LifestyleTag,EducationTag,IntegrationTag,IntegrationTag,MoviesTag,RatingTag]
testObject_ServiceTagList_provider_2 :: ServiceTagList
testObject_ServiceTagList_provider_2 = ServiceTagList [SportsTag,GraphicsTag,GamesTag,VideoTag,DesignTag,ShoppingTag,AudioTag,HealthTag,WeatherTag,TutorialTag,LifestyleTag,LifestyleTag,IntegrationTag,IntegrationTag,MoviesTag,FinanceTag,FoodDrinkTag,MedicalTag,MusicTag,ShoppingTag,EntertainmentTag,QuizTag,IntegrationTag,MoviesTag,DesignTag,MediaTag,RatingTag,MusicTag,FoodDrinkTag,GamesTag]
testObject_ServiceTagList_provider_3 :: ServiceTagList
testObject_ServiceTagList_provider_3 = ServiceTagList [WeatherTag]
testObject_ServiceTagList_provider_4 :: ServiceTagList
testObject_ServiceTagList_provider_4 = ServiceTagList [RatingTag,TutorialTag,AudioTag,PhotographyTag,EntertainmentTag,FinanceTag,IntegrationTag,WeatherTag,PollTag,SocialTag,WeatherTag,FinanceTag,TutorialTag,DesignTag,TravelTag,GamesTag,HealthTag,IntegrationTag,SportsTag,GraphicsTag,VideoTag,FoodDrinkTag,RatingTag]
testObject_ServiceTagList_provider_5 :: ServiceTagList
testObject_ServiceTagList_provider_5 = ServiceTagList [HealthTag]
testObject_ServiceTagList_provider_6 :: ServiceTagList
testObject_ServiceTagList_provider_6 = ServiceTagList [FinanceTag]
testObject_ServiceTagList_provider_7 :: ServiceTagList
testObject_ServiceTagList_provider_7 = ServiceTagList [QuizTag,LifestyleTag,MedicalTag,GamesTag,IntegrationTag,NewsTag,AudioTag,LifestyleTag,EducationTag,MusicTag,GraphicsTag,TutorialTag,NewsTag,MoviesTag,AudioTag,IntegrationTag,PollTag,TutorialTag,VideoTag,HealthTag,VideoTag,SocialTag,IntegrationTag]
testObject_ServiceTagList_provider_8 :: ServiceTagList
testObject_ServiceTagList_provider_8 = ServiceTagList [DesignTag,PhotographyTag,NewsTag,GraphicsTag,QuizTag,PhotographyTag,LifestyleTag,EducationTag,NewsTag,TutorialTag,TutorialTag,MediaTag,BusinessTag,TravelTag,SportsTag,SportsTag,LifestyleTag,EducationTag,MedicalTag,ShoppingTag,VideoTag,ShoppingTag,TravelTag,BusinessTag,NewsTag,TravelTag,MedicalTag]
testObject_ServiceTagList_provider_9 :: ServiceTagList
testObject_ServiceTagList_provider_9 = ServiceTagList [MedicalTag,FinanceTag,PollTag,AudioTag,AudioTag,MusicTag,RatingTag,SportsTag,ShoppingTag,ProductivityTag,ProductivityTag,WeatherTag,FoodDrinkTag,AudioTag,BusinessTag,ProductivityTag,RatingTag,HealthTag,PhotographyTag,FitnessTag,ProductivityTag,ShoppingTag,TravelTag,WeatherTag,SportsTag,FoodDrinkTag,EducationTag,SocialTag,BooksTag,VideoTag]
testObject_ServiceTagList_provider_10 :: ServiceTagList
testObject_ServiceTagList_provider_10 = ServiceTagList [PhotographyTag,BusinessTag,LifestyleTag,EntertainmentTag,MoviesTag,GraphicsTag,VideoTag,FinanceTag,ShoppingTag,MusicTag,FoodDrinkTag,NewsTag,PollTag,SocialTag,IntegrationTag,QuizTag,AudioTag,FinanceTag,DesignTag,DesignTag,MedicalTag,FitnessTag,PhotographyTag,MedicalTag]
testObject_ServiceTagList_provider_11 :: ServiceTagList
testObject_ServiceTagList_provider_11 = ServiceTagList []
testObject_ServiceTagList_provider_12 :: ServiceTagList
testObject_ServiceTagList_provider_12 = ServiceTagList [NewsTag,EducationTag,MusicTag,BusinessTag,AudioTag,VideoTag,BusinessTag,IntegrationTag,HealthTag,FitnessTag,BooksTag,SportsTag,FinanceTag,PhotographyTag,WeatherTag,SportsTag,FitnessTag,MoviesTag,FoodDrinkTag,EducationTag,SocialTag,AudioTag,QuizTag,GraphicsTag,FinanceTag,FinanceTag,EntertainmentTag]
testObject_ServiceTagList_provider_13 :: ServiceTagList
testObject_ServiceTagList_provider_13 = ServiceTagList [MedicalTag,SocialTag,DesignTag,MedicalTag,PollTag,FoodDrinkTag,EducationTag,AudioTag,LifestyleTag,VideoTag,ProductivityTag,BusinessTag,FoodDrinkTag,QuizTag,QuizTag,MoviesTag,IntegrationTag,AudioTag,GraphicsTag,FinanceTag,PollTag]
testObject_ServiceTagList_provider_14 :: ServiceTagList
testObject_ServiceTagList_provider_14 = ServiceTagList [MedicalTag,FoodDrinkTag,DesignTag,WeatherTag,TutorialTag,GraphicsTag,ShoppingTag]
testObject_ServiceTagList_provider_15 :: ServiceTagList
testObject_ServiceTagList_provider_15 = ServiceTagList [MoviesTag,MusicTag,EntertainmentTag,EducationTag,MusicTag,MusicTag,SportsTag,EntertainmentTag,BooksTag,FoodDrinkTag,GamesTag,FoodDrinkTag,PollTag,GamesTag,IntegrationTag,FinanceTag,TutorialTag,RatingTag,MusicTag,FinanceTag,FinanceTag,FinanceTag]
testObject_ServiceTagList_provider_16 :: ServiceTagList
testObject_ServiceTagList_provider_16 = ServiceTagList [PollTag,GraphicsTag,TravelTag,TutorialTag,MusicTag,EducationTag,HealthTag,DesignTag,EntertainmentTag,FinanceTag,SportsTag,SportsTag,QuizTag,QuizTag]
testObject_ServiceTagList_provider_17 :: ServiceTagList
testObject_ServiceTagList_provider_17 = ServiceTagList [GamesTag,GamesTag,RatingTag,FoodDrinkTag,IntegrationTag,LifestyleTag,HealthTag,WeatherTag,RatingTag]
testObject_ServiceTagList_provider_18 :: ServiceTagList
testObject_ServiceTagList_provider_18 = ServiceTagList [QuizTag,MoviesTag,RatingTag,GraphicsTag,ShoppingTag,SportsTag,EducationTag]
testObject_ServiceTagList_provider_19 :: ServiceTagList
testObject_ServiceTagList_provider_19 = ServiceTagList [BooksTag,MedicalTag,MoviesTag,MusicTag,EducationTag,PollTag,ShoppingTag,PollTag,MusicTag,DesignTag,MediaTag,QuizTag,FoodDrinkTag,ShoppingTag]
testObject_ServiceTagList_provider_20 :: ServiceTagList
testObject_ServiceTagList_provider_20 = ServiceTagList [ProductivityTag,PhotographyTag,SocialTag,MediaTag,PollTag,MusicTag,NewsTag,GamesTag,MusicTag,DesignTag,SocialTag,BusinessTag,WeatherTag,BusinessTag,MoviesTag,IntegrationTag,VideoTag,PollTag,BusinessTag,NewsTag,BusinessTag,TravelTag,TravelTag]
