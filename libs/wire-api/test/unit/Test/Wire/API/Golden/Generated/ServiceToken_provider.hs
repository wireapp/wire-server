{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ServiceToken_provider where

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
testObject_ServiceToken_provider_1 :: ServiceToken
testObject_ServiceToken_provider_1 = ServiceToken (fromRight undefined (validate ("_Xb4TMCS7tzlZl4=")))
testObject_ServiceToken_provider_2 :: ServiceToken
testObject_ServiceToken_provider_2 = ServiceToken (fromRight undefined (validate ("R6ycA-kPNDRGyw==")))
testObject_ServiceToken_provider_3 :: ServiceToken
testObject_ServiceToken_provider_3 = ServiceToken (fromRight undefined (validate ("aQP9KFaYsJds")))
testObject_ServiceToken_provider_4 :: ServiceToken
testObject_ServiceToken_provider_4 = ServiceToken (fromRight undefined (validate ("rmPLng==")))
testObject_ServiceToken_provider_5 :: ServiceToken
testObject_ServiceToken_provider_5 = ServiceToken (fromRight undefined (validate ("_vw4JFWJFX83NSQrw0l8AWe5Oa-S0xWUs3-TaiU=")))
testObject_ServiceToken_provider_6 :: ServiceToken
testObject_ServiceToken_provider_6 = ServiceToken (fromRight undefined (validate ("nxqu57Aq59KX")))
testObject_ServiceToken_provider_7 :: ServiceToken
testObject_ServiceToken_provider_7 = ServiceToken (fromRight undefined (validate ("mJKGMsQSNSoGhqzW")))
testObject_ServiceToken_provider_8 :: ServiceToken
testObject_ServiceToken_provider_8 = ServiceToken (fromRight undefined (validate ("UHEUGhgtLHU=")))
testObject_ServiceToken_provider_9 :: ServiceToken
testObject_ServiceToken_provider_9 = ServiceToken (fromRight undefined (validate ("VTIK")))
testObject_ServiceToken_provider_10 :: ServiceToken
testObject_ServiceToken_provider_10 = ServiceToken (fromRight undefined (validate ("9LLaDcvne6fnD6L0o0iP_gsD-gM=")))
testObject_ServiceToken_provider_11 :: ServiceToken
testObject_ServiceToken_provider_11 = ServiceToken (fromRight undefined (validate ("MsaVD81THZ5-tqp6n1rzkenmqkGd")))
testObject_ServiceToken_provider_12 :: ServiceToken
testObject_ServiceToken_provider_12 = ServiceToken (fromRight undefined (validate ("Q_vQsIcNhzfrp41_urm4B1g=")))
testObject_ServiceToken_provider_13 :: ServiceToken
testObject_ServiceToken_provider_13 = ServiceToken (fromRight undefined (validate ("0T11ibYj_M46mxIgzul8_XLEkm_C")))
testObject_ServiceToken_provider_14 :: ServiceToken
testObject_ServiceToken_provider_14 = ServiceToken (fromRight undefined (validate ("6-qf")))
testObject_ServiceToken_provider_15 :: ServiceToken
testObject_ServiceToken_provider_15 = ServiceToken (fromRight undefined (validate ("leeHyOkQooaFgpmEuUCMTuKoWfEV0viJ6qn8")))
testObject_ServiceToken_provider_16 :: ServiceToken
testObject_ServiceToken_provider_16 = ServiceToken (fromRight undefined (validate ("")))
testObject_ServiceToken_provider_17 :: ServiceToken
testObject_ServiceToken_provider_17 = ServiceToken (fromRight undefined (validate ("-IT4SBLtVBSr5Q3egg==")))
testObject_ServiceToken_provider_18 :: ServiceToken
testObject_ServiceToken_provider_18 = ServiceToken (fromRight undefined (validate ("1nMRh2lqKOT2SpvX330cjmwhiFk=")))
testObject_ServiceToken_provider_19 :: ServiceToken
testObject_ServiceToken_provider_19 = ServiceToken (fromRight undefined (validate ("tUqLMJ0=")))
testObject_ServiceToken_provider_20 :: ServiceToken
testObject_ServiceToken_provider_20 = ServiceToken (fromRight undefined (validate ("Lz2BAKBqiSA=")))
