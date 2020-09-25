{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

import Brig.Types
import Brig.Types.Intra
import Brig.Types.Provider ()
import Cassandra.CQL
import Control.Error (note)
import Data.Aeson (eitherDecode, encode)
import qualified Data.Aeson as JSON
import Data.Domain (Domain, domainText, mkDomain)
import Data.Handle (Handle (..))
import Data.Id ()
import Data.Range ()
import Data.String.Conversions (LBS, ST, cs)
import Data.Text.Ascii ()
import Imports
import Wire.API.User.RichInfo

deriving instance Cql Name

deriving instance Cql Handle

deriving instance Cql ColourId

deriving instance Cql Phone

deriving instance Cql Message

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
    Just e -> return e
    Nothing -> Left "fromCql: Invalid email"
  fromCql _ = Left "fromCql: email: CqlText expected"

  toCql = toCql . fromEmail

instance Cql UserSSOId where
  ctype = Tagged TextColumn

  fromCql (CqlText t) = case eitherDecode $ cs t of
    Right i -> return i
    Left msg -> Left $ "fromCql: Invalid UserSSOId: " ++ msg
  fromCql _ = Left "fromCql: UserSSOId: CqlText expected"

  toCql = toCql . cs @LBS @ST . encode

instance Cql Relation where
  ctype = Tagged IntColumn

  fromCql (CqlInt i) = case i of
    0 -> return Accepted
    1 -> return Blocked
    2 -> return Pending
    3 -> return Ignored
    4 -> return Sent
    5 -> return Cancelled
    n -> Left $ "unexpected relation: " ++ show n
  fromCql _ = Left "relation: int expected"

  toCql Accepted = CqlInt 0
  toCql Blocked = CqlInt 1
  toCql Pending = CqlInt 2
  toCql Ignored = CqlInt 3
  toCql Sent = CqlInt 4
  toCql Cancelled = CqlInt 5

-- DEPRECATED
instance Cql Pict where
  ctype = Tagged (ListColumn BlobColumn)

  fromCql (CqlList l) = do
    vs <- map (\(Blob lbs) -> lbs) <$> mapM fromCql l
    as <- mapM (note "Failed to read asset" . JSON.decode) vs
    return $ Pict as
  fromCql _ = return noPict

  toCql = toCql . map (Blob . JSON.encode) . fromPict

instance Cql AssetSize where
  ctype = Tagged IntColumn

  fromCql (CqlInt 0) = return AssetPreview
  fromCql (CqlInt 1) = return AssetComplete
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
      0 -> return $! ImageAsset k s
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
        ("key", CqlText k),
        ("size", toCql s)
      ]

instance Cql AccountStatus where
  ctype = Tagged IntColumn

  toCql Active'182 = CqlInt 0
  toCql Suspended'182 = CqlInt 1
  toCql Deleted'182 = CqlInt 2
  toCql Ephemeral'182 = CqlInt 3
  toCql PendingInvitation = CqlInt 4

  fromCql (CqlInt i) = case i of
    0 -> return Active'182
    1 -> return Suspended'182
    2 -> return Deleted'182
    3 -> return Ephemeral'182
    4 -> return PendingInvitation
    n -> Left $ "unexpected account status: " ++ show n
  fromCql _ = Left "account status: int expected"

instance Cql ClientType where
  ctype = Tagged IntColumn
  toCql TemporaryClientType = CqlInt 0
  toCql PermanentClientType = CqlInt 1
  toCql LegalHoldClientType = CqlInt 2

  fromCql (CqlInt 0) = return TemporaryClientType
  fromCql (CqlInt 1) = return PermanentClientType
  fromCql (CqlInt 2) = return LegalHoldClientType
  fromCql _ = Left "ClientType: Int [0, 2] expected"

instance Cql ClientClass where
  ctype = Tagged IntColumn
  toCql PhoneClient = CqlInt 0
  toCql TabletClient = CqlInt 1
  toCql DesktopClient = CqlInt 2
  toCql LegalHoldClient = CqlInt 3

  fromCql (CqlInt 0) = return PhoneClient
  fromCql (CqlInt 1) = return TabletClient
  fromCql (CqlInt 2) = return DesktopClient
  fromCql (CqlInt 3) = return LegalHoldClient
  fromCql _ = Left "ClientClass: Int [0, 3] expected"

instance Cql PropertyValue where
  ctype = Tagged BlobColumn
  toCql = toCql . Blob . JSON.encode . propertyValueJson
  fromCql (CqlBlob v) = case JSON.eitherDecode v of
    Left e -> Left ("Failed to read property value: " <> e)
    Right x -> pure (PropertyValue x)
  fromCql _ = Left "PropertyValue: Blob expected"

instance Cql Country where
  ctype = Tagged AsciiColumn
  toCql = toCql . con2Text

  fromCql (CqlAscii c) = case parseCountry c of
    Just c' -> return c'
    Nothing -> Left "Country: ISO 3166-1-alpha2 expected."
  fromCql _ = Left "Country: ASCII expected"

instance Cql Language where
  ctype = Tagged AsciiColumn
  toCql = toCql . lan2Text

  fromCql (CqlAscii l) = case parseLanguage l of
    Just l' -> return l'
    Nothing -> Left "Language: ISO 639-1 expected."
  fromCql _ = Left "Language: ASCII expected"

instance Cql ManagedBy where
  ctype = Tagged IntColumn

  fromCql (CqlInt 0) = return ManagedByWire
  fromCql (CqlInt 1) = return ManagedByScim
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
