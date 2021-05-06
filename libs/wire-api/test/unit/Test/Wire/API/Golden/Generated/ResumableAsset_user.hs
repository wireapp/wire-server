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
testObject_ResumableAsset_user_1 :: ResumableAsset
testObject_ResumableAsset_user_1 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-11 13:12:20.267034498181 UTC")) & assetToken .~ Nothing)) (read "1864-04-29 18:19:53.565309024954 UTC") (ChunkSize {chunkSizeBytes = 3}))
testObject_ResumableAsset_user_2 :: ResumableAsset
testObject_ResumableAsset_user_2 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-08 06:55:05.322980453085 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("fGM=")))}))) (read "1864-05-17 19:02:53.428954556293 UTC") (ChunkSize {chunkSizeBytes = 10}))
testObject_ResumableAsset_user_3 :: ResumableAsset
testObject_ResumableAsset_user_3 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-05-08 05:18:42.598550550672 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("Dw==")))}))) (read "1864-05-04 10:50:59.591889487965 UTC") (ChunkSize {chunkSizeBytes = 0}))
testObject_ResumableAsset_user_4 :: ResumableAsset
testObject_ResumableAsset_user_4 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Nothing)) (read "1864-05-17 16:52:38.110671457528 UTC") (ChunkSize {chunkSizeBytes = 10}))
testObject_ResumableAsset_user_5 :: ResumableAsset
testObject_ResumableAsset_user_5 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-06 00:12:35.853496120639 UTC")) & assetToken .~ Nothing)) (read "1864-04-30 15:31:23.939869916452 UTC") (ChunkSize {chunkSizeBytes = 0}))
