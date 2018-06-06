{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Brig.Data.Instances () where

import Brig.Types
import Brig.Types.Intra
import Brig.Types.Provider ()
import Cassandra.CQL
import Control.Error (note)
import Data.Aeson (eitherDecode, encode)
import Data.Id()
import Data.Int
import Data.Range()
import Data.String.Conversions (cs, LBS, ST)
import Data.Text.Ascii()

import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Parser               as JSON
import qualified Data.Attoparsec.ByteString.Lazy as P

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
deriving instance Cql SearchableStatus

instance Cql Email where
    ctype = Tagged TextColumn

    fromCql (CqlText t) = case parseEmail t of
        Just  e -> return e
        Nothing -> fail "fromCql: Invalid email"
    fromCql _           = fail "fromCql: email: CqlText expected"

    toCql = toCql . fromEmail

instance Cql UserSSOId where
    ctype = Tagged TextColumn

    fromCql (CqlText t) = case eitherDecode $ cs t of
        Right i  -> return i
        Left msg -> fail $ "fromCql: Invalid UserSSOId: " ++ msg
    fromCql _           = fail "fromCql: UserSSOId: CqlText expected"

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
        n -> fail $ "unexpected relation: " ++ show n
    fromCql _ = fail "relation: int expected"

    toCql Accepted  = CqlInt 0
    toCql Blocked   = CqlInt 1
    toCql Pending   = CqlInt 2
    toCql Ignored   = CqlInt 3
    toCql Sent      = CqlInt 4
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
    fromCql n = fail $ "Unexpected asset size: " ++ show n

    toCql AssetPreview  = CqlInt 0
    toCql AssetComplete = CqlInt 1

instance Cql Asset where
    -- Note: Type name and column names and types must match up with the
    --       Cassandra schema definition. New fields may only be added
    --       (appended) but no fields may be removed.
    ctype = Tagged (UdtColumn "asset"
          [ ("typ", IntColumn)
          , ("key", TextColumn)
          , ("size", MaybeColumn IntColumn)
          ])

    fromCql (CqlUdt fs) = do
        t <- required "typ"
        k <- required "key"
        s <- optional "size"
        case (t :: Int32) of
            0 -> return $! ImageAsset k s
            _ -> fail $ "unexpected user asset type: " ++ show t
      where
        required f = maybe (fail ("Asset: Missing required field '" ++ show f ++ "'"))
                           fromCql
                           (lookup f fs)

        optional f = maybe (Right Nothing) fromCql (lookup f fs)

    fromCql _ = fail "UserAsset: UDT expected"

    -- Note: Order must match up with the 'ctype' definition.
    toCql (ImageAsset k s) = CqlUdt
        [ ("typ", CqlInt 0)
        , ("key", CqlText k)
        , ("size", toCql s)
        ]

instance Cql AccountStatus where
    ctype = Tagged IntColumn

    toCql Active    = CqlInt 0
    toCql Suspended = CqlInt 1
    toCql Deleted   = CqlInt 2
    toCql Ephemeral = CqlInt 3

    fromCql (CqlInt i) = case i of
        0 -> return Active
        1 -> return Suspended
        2 -> return Deleted
        3 -> return Ephemeral
        n -> fail $ "unexpected account status: " ++ show n
    fromCql _ = fail "account status: int expected"

instance Cql ClientType where
    ctype = Tagged IntColumn
    toCql TemporaryClient = CqlInt 0
    toCql PermanentClient = CqlInt 1

    fromCql (CqlInt 0) = return TemporaryClient
    fromCql (CqlInt 1) = return PermanentClient
    fromCql _          = fail "ClientType: Int [0, 1] expected"

instance Cql ClientClass where
    ctype = Tagged IntColumn
    toCql PhoneClient   = CqlInt 0
    toCql TabletClient  = CqlInt 1
    toCql DesktopClient = CqlInt 2

    fromCql (CqlInt 0) = return PhoneClient
    fromCql (CqlInt 1) = return TabletClient
    fromCql (CqlInt 2) = return DesktopClient
    fromCql _          = fail "ClientClass: Int [0, 2] expected"

instance Cql PrekeyId where
    ctype = Tagged IntColumn
    toCql = CqlInt . fromIntegral . keyId
    fromCql (CqlInt i) = return $ PrekeyId (fromIntegral i)
    fromCql _            = fail "PrekeyId: Int expected"

instance Cql PropertyValue where
    ctype = Tagged BlobColumn
    toCql = toCql . Blob . JSON.encode . propertyValueJson
    fromCql (CqlBlob v)  = note "Failed to read property value" $ fmap PropertyValue (P.maybeResult (P.parse JSON.value v))
    fromCql _            = fail "PropertyValue: Blob expected"

instance Cql Country where
    ctype = Tagged AsciiColumn
    toCql = toCql . con2Text

    fromCql (CqlAscii c) = case parseCountry c of
        Just c' -> return c'
        Nothing -> fail "Country: ISO 3166-1-alpha2 expected."
    fromCql _            = fail "Country: ASCII expected"

instance Cql Language where
    ctype = Tagged AsciiColumn
    toCql = toCql . lan2Text

    fromCql (CqlAscii l) = case parseLanguage l of
        Just l' -> return l'
        Nothing -> fail "Language: ISO 639-1 expected."
    fromCql _            = fail "Language: ASCII expected"
