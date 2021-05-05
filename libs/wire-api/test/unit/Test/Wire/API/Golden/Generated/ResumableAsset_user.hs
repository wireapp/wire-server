{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ResumableAsset_user where

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
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_ResumableAsset_1 :: ResumableAsset
testObject_ResumableAsset_1 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-06 19:30:10.650310787415 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))) (read "1864-05-12 11:42:40.539154278048 UTC") (ChunkSize {chunkSizeBytes = 5}))
testObject_ResumableAsset_2 :: ResumableAsset
testObject_ResumableAsset_2 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("HQ==")))}))) (read "1864-05-03 01:39:26.037210991295 UTC") (ChunkSize {chunkSizeBytes = 6}))
testObject_ResumableAsset_3 :: ResumableAsset
testObject_ResumableAsset_3 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) AssetVolatile) & assetExpires .~ (fmap read (Just "1864-05-09 15:55:12.972890358279 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("")))}))) (read "1864-05-13 01:41:08.489830171159 UTC") (ChunkSize {chunkSizeBytes = 10}))
testObject_ResumableAsset_4 :: ResumableAsset
testObject_ResumableAsset_4 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-07 12:55:26.058456587452 UTC")) & assetToken .~ Nothing)) (read "1864-05-15 19:36:19.424049524459 UTC") (ChunkSize {chunkSizeBytes = 4}))
testObject_ResumableAsset_5 :: ResumableAsset
testObject_ResumableAsset_5 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) AssetPersistent) & assetExpires .~ (fmap read (Just "1864-05-11 08:25:06.124057116288 UTC")) & assetToken .~ Nothing)) (read "1864-05-11 20:39:52.223802674663 UTC") (ChunkSize {chunkSizeBytes = 8}))
