{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Brig.Data.Instances
  (
  )
where

import Brig.Types.Common
import Brig.Types.Search
import Cassandra.CQL
import Control.Error (note)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson qualified as JSON
import Data.ByteString.Conversion
import Data.Domain (Domain, domainText, mkDomain)
import Data.Handle (Handle (..))
import Data.Id ()
import Data.Range ()
import Data.Text.Ascii ()
import Data.Text.Encoding (encodeUtf8)
import Imports
import Wire.API.Asset (AssetKey, assetKeyToText, nilAssetKey)
import Wire.API.Connection (RelationWithHistory (..))
import Wire.API.MLS.CipherSuite
import Wire.API.Properties
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Client
import Wire.API.User.Password
import Wire.API.User.RichInfo
import Wire.API.User.Search

deriving instance Cql Name

deriving instance Cql Handle

deriving instance Cql ColourId

deriving instance Cql Phone

deriving instance Cql InvitationCode

deriving instance Cql PasswordResetKey

deriving instance Cql PasswordResetCode

deriving instance Cql ActivationKey

deriving instance Cql ActivationCode

deriving instance Cql PropertyKey

deriving instance Cql PhonePrefix

instance Cql Email where
  ctype = Tagged TextColumn

  fromCql (CqlText t) = case parseEmail t of
    Just e -> pure e
    Nothing -> Left "fromCql: Invalid email"
  fromCql _ = Left "fromCql: email: CqlText expected"

  toCql = toCql . fromEmail

instance Cql UserSSOId where
  ctype = Tagged TextColumn

  fromCql (CqlText t) = case eitherDecode $ cs t of
    Right i -> pure i
    Left msg -> Left $ "fromCql: Invalid UserSSOId: " ++ msg
  fromCql _ = Left "fromCql: UserSSOId: CqlText expected"

  toCql = toCql . cs @LByteString @Text . encode

instance Cql RelationWithHistory where
  ctype = Tagged IntColumn

  fromCql (CqlInt i) = case i of
    0 -> pure AcceptedWithHistory
    1 -> pure BlockedWithHistory
    2 -> pure PendingWithHistory
    3 -> pure IgnoredWithHistory
    4 -> pure SentWithHistory
    5 -> pure CancelledWithHistory
    6 -> pure MissingLegalholdConsentFromAccepted
    7 -> pure MissingLegalholdConsentFromBlocked
    8 -> pure MissingLegalholdConsentFromPending
    9 -> pure MissingLegalholdConsentFromIgnored
    10 -> pure MissingLegalholdConsentFromSent
    11 -> pure MissingLegalholdConsentFromCancelled
    n -> Left $ "unexpected RelationWithHistory: " ++ show n
  fromCql _ = Left "RelationWithHistory: int expected"

  toCql AcceptedWithHistory = CqlInt 0
  toCql BlockedWithHistory = CqlInt 1
  toCql PendingWithHistory = CqlInt 2
  toCql IgnoredWithHistory = CqlInt 3
  toCql SentWithHistory = CqlInt 4
  toCql CancelledWithHistory = CqlInt 5
  toCql MissingLegalholdConsentFromAccepted = CqlInt 6
  toCql MissingLegalholdConsentFromBlocked = CqlInt 7
  toCql MissingLegalholdConsentFromPending = CqlInt 8
  toCql MissingLegalholdConsentFromIgnored = CqlInt 9
  toCql MissingLegalholdConsentFromSent = CqlInt 10
  toCql MissingLegalholdConsentFromCancelled = CqlInt 11

-- DEPRECATED
instance Cql Pict where
  ctype = Tagged (ListColumn BlobColumn)

  fromCql (CqlList l) = do
    vs <- map (\(Blob lbs) -> lbs) <$> mapM fromCql l
    as <- mapM (note "Failed to read asset" . JSON.decode) vs
    pure $ Pict as
  fromCql _ = pure noPict

  toCql = toCql . map (Blob . JSON.encode) . fromPict

instance Cql AssetKey where
  ctype = Tagged TextColumn
  toCql = CqlText . assetKeyToText

  -- if the asset key is invalid we will return the nil asset key (`3-1-00000000-0000-0000-0000-000000000000`)
  fromCql (CqlText txt) = pure $ fromRight nilAssetKey $ runParser parser $ encodeUtf8 txt
  fromCql _ = Left "AssetKey: Expected CqlText"

instance Cql AssetSize where
  ctype = Tagged IntColumn

  fromCql (CqlInt 0) = pure AssetPreview
  fromCql (CqlInt 1) = pure AssetComplete
  fromCql n = Left $ "Unexpected asset size: " ++ show n

  toCql AssetPreview = CqlInt 0
  toCql AssetComplete = CqlInt 1

instance Cql Asset where
  -- Note: Type name and column names and types must match up with the
  --       Cassandra schema definition. New fields may only be added
  --       (appended) but no fields may be removed.
  ctype =
    Tagged
      ( UdtColumn
          "asset"
          [ ("typ", IntColumn),
            ("key", TextColumn),
            ("size", MaybeColumn IntColumn)
          ]
      )

  fromCql (CqlUdt fs) = do
    t <- required "typ"
    k <- required "key"
    s <- optional "size"
    case (t :: Int32) of
      0 -> pure $! ImageAsset k s
      _ -> Left $ "unexpected user asset type: " ++ show t
    where
      required :: Cql r => Text -> Either String r
      required f =
        maybe
          (Left ("Asset: Missing required field '" ++ show f ++ "'"))
          fromCql
          (lookup f fs)
      optional f = maybe (Right Nothing) fromCql (lookup f fs)
  fromCql _ = Left "UserAsset: UDT expected"

  -- Note: Order must match up with the 'ctype' definition.
  toCql (ImageAsset k s) =
    CqlUdt
      [ ("typ", CqlInt 0),
        ("key", toCql k),
        ("size", toCql s)
      ]

instance Cql AccountStatus where
  ctype = Tagged IntColumn

  toCql Active = CqlInt 0
  toCql Suspended = CqlInt 1
  toCql Deleted = CqlInt 2
  toCql Ephemeral = CqlInt 3
  toCql PendingInvitation = CqlInt 4

  fromCql (CqlInt i) = case i of
    0 -> pure Active
    1 -> pure Suspended
    2 -> pure Deleted
    3 -> pure Ephemeral
    4 -> pure PendingInvitation
    n -> Left $ "unexpected account status: " ++ show n
  fromCql _ = Left "account status: int expected"

instance Cql ClientType where
  ctype = Tagged IntColumn
  toCql TemporaryClientType = CqlInt 0
  toCql PermanentClientType = CqlInt 1
  toCql LegalHoldClientType = CqlInt 2

  fromCql (CqlInt 0) = pure TemporaryClientType
  fromCql (CqlInt 1) = pure PermanentClientType
  fromCql (CqlInt 2) = pure LegalHoldClientType
  fromCql _ = Left "ClientType: Int [0, 2] expected"

instance Cql ClientClass where
  ctype = Tagged IntColumn
  toCql PhoneClient = CqlInt 0
  toCql TabletClient = CqlInt 1
  toCql DesktopClient = CqlInt 2
  toCql LegalHoldClient = CqlInt 3

  fromCql (CqlInt 0) = pure PhoneClient
  fromCql (CqlInt 1) = pure TabletClient
  fromCql (CqlInt 2) = pure DesktopClient
  fromCql (CqlInt 3) = pure LegalHoldClient
  fromCql _ = Left "ClientClass: Int [0, 3] expected"

instance Cql RawPropertyValue where
  ctype = Tagged BlobColumn
  toCql = toCql . Blob . rawPropertyBytes
  fromCql (CqlBlob v) = pure (RawPropertyValue v)
  fromCql _ = Left "PropertyValue: Blob expected"

instance Cql Country where
  ctype = Tagged AsciiColumn
  toCql = toCql . con2Text

  fromCql (CqlAscii c) = case parseCountry c of
    Just c' -> pure c'
    Nothing -> Left "Country: ISO 3166-1-alpha2 expected."
  fromCql _ = Left "Country: ASCII expected"

instance Cql Language where
  ctype = Tagged AsciiColumn
  toCql = toCql . lan2Text

  fromCql (CqlAscii l) = case parseLanguage l of
    Just l' -> pure l'
    Nothing -> Left "Language: ISO 639-1 expected."
  fromCql _ = Left "Language: ASCII expected"

instance Cql ManagedBy where
  ctype = Tagged IntColumn

  fromCql (CqlInt 0) = pure ManagedByWire
  fromCql (CqlInt 1) = pure ManagedByScim
  fromCql n = Left $ "Unexpected ManagedBy: " ++ show n

  toCql ManagedByWire = CqlInt 0
  toCql ManagedByScim = CqlInt 1

instance Cql RichInfoAssocList where
  ctype = Tagged BlobColumn
  toCql = toCql . Blob . JSON.encode
  fromCql (CqlBlob v) = JSON.eitherDecode v
  fromCql _ = Left "RichInfo: Blob expected"

instance Cql Domain where
  ctype = Tagged TextColumn
  toCql = CqlText . domainText
  fromCql (CqlText txt) = mkDomain txt
  fromCql _ = Left "Domain: Text expected"

instance Cql SearchVisibilityInbound where
  ctype = Tagged IntColumn

  toCql SearchableByOwnTeam = CqlInt 0
  toCql SearchableByAllTeams = CqlInt 1

  fromCql (CqlInt 0) = pure SearchableByOwnTeam
  fromCql (CqlInt 1) = pure SearchableByAllTeams
  fromCql n = Left $ "Unexpected SearchVisibilityInbound: " ++ show n

instance Cql FederatedUserSearchPolicy where
  ctype = Tagged IntColumn

  toCql NoSearch = CqlInt 0
  toCql ExactHandleSearch = CqlInt 1
  toCql FullSearch = CqlInt 2

  fromCql (CqlInt 0) = pure NoSearch
  fromCql (CqlInt 1) = pure ExactHandleSearch
  fromCql (CqlInt 2) = pure FullSearch
  fromCql n = Left $ "Unexpected SearchVisibilityInbound: " ++ show n

instance Cql (Imports.Set BaseProtocolTag) where
  ctype = Tagged IntColumn

  toCql = CqlInt . fromIntegral . protocolSetBits
  fromCql (CqlInt bits) = pure $ protocolSetFromBits (fromIntegral bits)
  fromCql _ = Left "Protocol set: Int expected"

instance Cql CipherSuiteTag where
  ctype = Tagged IntColumn
  toCql = CqlInt . fromIntegral . cipherSuiteNumber . tagCipherSuite

  fromCql (CqlInt index) =
    case cipherSuiteTag (CipherSuite (fromIntegral index)) of
      Just tag -> Right tag
      Nothing -> Left "CipherSuiteTag: unexpected index"
  fromCql _ = Left "CipherSuiteTag: int expected"
