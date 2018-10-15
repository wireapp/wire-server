{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gundeck.Instances () where

import Imports
import Cassandra.CQL
import Data.Id
import Gundeck.Aws.Arn (EndpointArn)
import Gundeck.Types
import Network.AWS.Data

import qualified Data.UUID            as Uuid
import qualified Data.Attoparsec.Text as Parser
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.Text.Encoding   as Text

instance Cql Transport where
    ctype = Tagged IntColumn

    toCql GCM             = CqlInt 0
    toCql APNS            = CqlInt 1
    toCql APNSSandbox     = CqlInt 2
    toCql APNSVoIP        = CqlInt 3
    toCql APNSVoIPSandbox = CqlInt 4

    fromCql (CqlInt i) = case i of
        0 -> return GCM
        1 -> return APNS
        2 -> return APNSSandbox
        3 -> return APNSVoIP
        4 -> return APNSVoIPSandbox
        n -> fail $ "unexpected transport: " ++ show n
    fromCql _ = fail "transport: int expected"

instance Cql ConnId where
    ctype = Tagged BlobColumn

    toCql (ConnId c) = CqlBlob (Bytes.fromStrict c)

    fromCql (CqlBlob b) = return . ConnId $ Bytes.toStrict b
    fromCql _           = fail "ConnId: Blob expected"

instance Cql EndpointArn where
    ctype = Tagged TextColumn
    toCql = CqlText . toText
    fromCql (CqlText txt) = either fail return (fromText txt)
    fromCql _             = fail "EndpointArn: Text expected"

instance Cql Token where
    ctype = Tagged TextColumn
    toCql = CqlText . tokenText
    fromCql (CqlText txt) = Right (Token txt)
    fromCql _             = fail "Token: Text expected"

instance Cql AppName where
    ctype = Tagged TextColumn
    toCql = CqlText . appNameText
    fromCql (CqlText txt) = Right (AppName txt)
    fromCql _             = fail "App: Text expected"

instance Cql EncKey where
    ctype = Tagged BlobColumn

    toCql (EncKey ek) = CqlBlob (Bytes.fromStrict ek)

    fromCql (CqlBlob b) = return . EncKey $ Bytes.toStrict b
    fromCql _ = fail "EncKey: blob expected"

instance Cql MacKey where
    ctype = Tagged BlobColumn

    toCql (MacKey mk) = CqlBlob (Bytes.fromStrict mk)

    fromCql (CqlBlob b) = return . MacKey $ Bytes.toStrict b
    fromCql _ = fail "MacKey: blob expected"

instance ToText (Id a) where
    toText = Text.decodeUtf8 . Uuid.toASCIIBytes . toUUID

instance FromText (Id a) where
    parser = Parser.take 36 >>= \txt ->
        txt & Text.encodeUtf8
            & Uuid.fromASCIIBytes
            & maybe (fail "Invalid UUID") (return . Id)
