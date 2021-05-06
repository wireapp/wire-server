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
testObject_ServiceToken_provider_1 = ServiceToken (fromRight undefined (validate ("jFs=")))
testObject_ServiceToken_provider_2 :: ServiceToken
testObject_ServiceToken_provider_2 = ServiceToken (fromRight undefined (validate ("mn3prFiZaUsi2bqUKSpvj9wGzA==")))
testObject_ServiceToken_provider_3 :: ServiceToken
testObject_ServiceToken_provider_3 = ServiceToken (fromRight undefined (validate ("7WauGVijHECshw_BO6TalCTZN9YD")))
testObject_ServiceToken_provider_4 :: ServiceToken
testObject_ServiceToken_provider_4 = ServiceToken (fromRight undefined (validate ("PIKRnK-UICGP7z-bJJ6NlcRHXCVbjgVh5yQ=")))
testObject_ServiceToken_provider_5 :: ServiceToken
testObject_ServiceToken_provider_5 = ServiceToken (fromRight undefined (validate ("4A57v2DpxjP5dzLb1r0ULFf6gQ==")))
testObject_ServiceToken_provider_6 :: ServiceToken
testObject_ServiceToken_provider_6 = ServiceToken (fromRight undefined (validate ("z9WjzGJKDPg=")))
testObject_ServiceToken_provider_7 :: ServiceToken
testObject_ServiceToken_provider_7 = ServiceToken (fromRight undefined (validate ("")))
testObject_ServiceToken_provider_8 :: ServiceToken
testObject_ServiceToken_provider_8 = ServiceToken (fromRight undefined (validate ("abUFc9c=")))
testObject_ServiceToken_provider_9 :: ServiceToken
testObject_ServiceToken_provider_9 = ServiceToken (fromRight undefined (validate ("VjMSzW8pTQz6QVp-L7aBMA==")))
testObject_ServiceToken_provider_10 :: ServiceToken
testObject_ServiceToken_provider_10 = ServiceToken (fromRight undefined (validate ("Ax9kwUfzdbyNRdLNvwWGuypcWcjqGafs")))
testObject_ServiceToken_provider_11 :: ServiceToken
testObject_ServiceToken_provider_11 = ServiceToken (fromRight undefined (validate ("ZL9WCn7TzsVIsCcQhqYPW-AuJw==")))
testObject_ServiceToken_provider_12 :: ServiceToken
testObject_ServiceToken_provider_12 = ServiceToken (fromRight undefined (validate ("zaa2QGhncA==")))
testObject_ServiceToken_provider_13 :: ServiceToken
testObject_ServiceToken_provider_13 = ServiceToken (fromRight undefined (validate ("ZZo=")))
testObject_ServiceToken_provider_14 :: ServiceToken
testObject_ServiceToken_provider_14 = ServiceToken (fromRight undefined (validate ("rWIS1f8omHf0G3MrOyltajYQUpU=")))
testObject_ServiceToken_provider_15 :: ServiceToken
testObject_ServiceToken_provider_15 = ServiceToken (fromRight undefined (validate ("WcXx")))
testObject_ServiceToken_provider_16 :: ServiceToken
testObject_ServiceToken_provider_16 = ServiceToken (fromRight undefined (validate ("9A==")))
testObject_ServiceToken_provider_17 :: ServiceToken
testObject_ServiceToken_provider_17 = ServiceToken (fromRight undefined (validate ("3lQY9Bhed_7hz9K0dSiZaPzzFQjpgH94JBk=")))
testObject_ServiceToken_provider_18 :: ServiceToken
testObject_ServiceToken_provider_18 = ServiceToken (fromRight undefined (validate ("")))
testObject_ServiceToken_provider_19 :: ServiceToken
testObject_ServiceToken_provider_19 = ServiceToken (fromRight undefined (validate ("epvuDymolvnglmBBmMk_SKM=")))
testObject_ServiceToken_provider_20 :: ServiceToken
testObject_ServiceToken_provider_20 = ServiceToken (fromRight undefined (validate ("h0essQ==")))
