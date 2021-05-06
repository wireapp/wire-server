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
testObject_ServiceToken_provider_1 = ServiceToken (fromRight undefined (validate ("ltfhVuVhZuM=")))
testObject_ServiceToken_provider_2 :: ServiceToken
testObject_ServiceToken_provider_2 = ServiceToken (fromRight undefined (validate ("OmRaCgAP7nFc")))
testObject_ServiceToken_provider_3 :: ServiceToken
testObject_ServiceToken_provider_3 = ServiceToken (fromRight undefined (validate ("v9OqV6vfx2R8XH9nNeUewBcQzEAxu_Y=")))
testObject_ServiceToken_provider_4 :: ServiceToken
testObject_ServiceToken_provider_4 = ServiceToken (fromRight undefined (validate ("zXNTlTHCwnwkXeiOgGYOaukRIezpSk6IMpVE")))
testObject_ServiceToken_provider_5 :: ServiceToken
testObject_ServiceToken_provider_5 = ServiceToken (fromRight undefined (validate ("MnSsYeXKkrutMTx8kzEbBV5PqI1Edw==")))
testObject_ServiceToken_provider_6 :: ServiceToken
testObject_ServiceToken_provider_6 = ServiceToken (fromRight undefined (validate ("dp_yjbptDqDsBmVZ0gTcm7gNpjVj9twiD3w=")))
testObject_ServiceToken_provider_7 :: ServiceToken
testObject_ServiceToken_provider_7 = ServiceToken (fromRight undefined (validate ("bum1wagTjtMjjGBKMCJ9biI=")))
testObject_ServiceToken_provider_8 :: ServiceToken
testObject_ServiceToken_provider_8 = ServiceToken (fromRight undefined (validate ("pCQw94hJ9fO-TCgdLm6s4nXy13Cs3TjgCA==")))
testObject_ServiceToken_provider_9 :: ServiceToken
testObject_ServiceToken_provider_9 = ServiceToken (fromRight undefined (validate ("wWreXrP6VoLEDjuMKZVEPfjg198d5xYxyaGm")))
testObject_ServiceToken_provider_10 :: ServiceToken
testObject_ServiceToken_provider_10 = ServiceToken (fromRight undefined (validate ("Je_eNPpgaRjv7rbKeMphN2g=")))
testObject_ServiceToken_provider_11 :: ServiceToken
testObject_ServiceToken_provider_11 = ServiceToken (fromRight undefined (validate ("n89qRDZDcJ870QdhYDu0kOJJ4w_t9jk=")))
testObject_ServiceToken_provider_12 :: ServiceToken
testObject_ServiceToken_provider_12 = ServiceToken (fromRight undefined (validate ("oEDi3Qrv2OyVXrGCjGJFfSLYdHozqfPl")))
testObject_ServiceToken_provider_13 :: ServiceToken
testObject_ServiceToken_provider_13 = ServiceToken (fromRight undefined (validate ("iswNT752PuavKJL_U15Vog==")))
testObject_ServiceToken_provider_14 :: ServiceToken
testObject_ServiceToken_provider_14 = ServiceToken (fromRight undefined (validate ("7Cvev4I7_IRY0xJs4Oh83HI3jn1bEYoz2ldr")))
testObject_ServiceToken_provider_15 :: ServiceToken
testObject_ServiceToken_provider_15 = ServiceToken (fromRight undefined (validate ("jvh9yg==")))
testObject_ServiceToken_provider_16 :: ServiceToken
testObject_ServiceToken_provider_16 = ServiceToken (fromRight undefined (validate ("DtJseZCVhNhK1ZGaCq8y2fdES2pWv89ss1E=")))
testObject_ServiceToken_provider_17 :: ServiceToken
testObject_ServiceToken_provider_17 = ServiceToken (fromRight undefined (validate ("ug==")))
testObject_ServiceToken_provider_18 :: ServiceToken
testObject_ServiceToken_provider_18 = ServiceToken (fromRight undefined (validate ("")))
testObject_ServiceToken_provider_19 :: ServiceToken
testObject_ServiceToken_provider_19 = ServiceToken (fromRight undefined (validate ("bxqWyd5AIx1RyafYtuYwNMh3cj3Qjw==")))
testObject_ServiceToken_provider_20 :: ServiceToken
testObject_ServiceToken_provider_20 = ServiceToken (fromRight undefined (validate ("73aTgZQt0g==")))
