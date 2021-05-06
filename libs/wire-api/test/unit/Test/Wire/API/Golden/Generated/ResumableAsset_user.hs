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
testObject_ResumableAsset_user_1 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000026-0000-0011-0000-003200000012"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-16 20:38:48.978 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("38ZNgLUeFaZCoA-NO8gZgaZRnLr2wF8puFpDfu4=")))}))) (read "1864-05-16 11:18:02.317 UTC") (ChunkSize {chunkSizeBytes = 25}))
testObject_ResumableAsset_user_2 :: ResumableAsset
testObject_ResumableAsset_user_2 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000007-0000-0078-0000-006f0000007b"))) AssetEternal) & assetExpires .~ (fmap read (Just "1864-04-21 18:44:39.54 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("mRB73hB_VgDvpA==")))}))) (read "1864-05-15 20:46:21.171 UTC") (ChunkSize {chunkSizeBytes = 1}))
testObject_ResumableAsset_user_3 :: ResumableAsset
testObject_ResumableAsset_user_3 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000005-0000-002e-0000-004e00000029"))) AssetEternal) & assetExpires .~ (fmap read (Nothing)) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("MD4cHYQAbds=")))}))) (read "1864-04-13 21:22:26.351 UTC") (ChunkSize {chunkSizeBytes = 2}))
testObject_ResumableAsset_user_4 :: ResumableAsset
testObject_ResumableAsset_user_4 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "0000001e-0000-0034-0000-002e00000078"))) AssetExpiring) & assetExpires .~ (fmap read (Just "1864-06-05 13:22:12.427 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("fHInB2ZrBqx9n7VOo6bUq_mKzC5nJTex3VHEqQ==")))}))) (read "1864-04-18 21:05:53.479 UTC") (ChunkSize {chunkSizeBytes = 20}))
testObject_ResumableAsset_user_5 :: ResumableAsset
testObject_ResumableAsset_user_5 = (mkResumableAsset ((mkAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "00000025-0000-0070-0000-000a00000052"))) AssetEternalInfrequentAccess) & assetExpires .~ (fmap read (Just "1864-05-08 05:00:50.242 UTC")) & assetToken .~ Just (AssetToken {assetTokenAscii = (fromRight undefined (validate ("8UGsYwA=")))}))) (read "1864-05-10 18:32:02.736 UTC") (ChunkSize {chunkSizeBytes = 20}))
