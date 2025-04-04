{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- FUTUREWORK: disallow orphans.

module SAML2.WebSSO.Orphans where

import Control.Exception (assert)
import Control.Monad ((<=<))
import Data.Aeson
import Data.ByteString
import Data.ByteString.Builder
import Data.Schema as Schema
import Data.String.Conversions
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.X509 as X509
import Data.Yaml.Aeson qualified as A
import SAML2.Util (normURI, parseURI', renderURI)
import Servant hiding (URI)
import System.Logger (Level (..))
import Text.XML.DSig
import URI.ByteString

instance FromJSON URI where
  parseJSON = (either unerror (pure . normURI) . parseURI') <=< parseJSON
    where
      unerror = fail . ("could not parse config: " <>) . show

instance ToJSON URI where
  toJSON = toJSON . renderURI

instance ToHttpApiData URI where
  toUrlPiece = renderURI

instance FromHttpApiData URI where
  parseUrlPiece = either (Left . Text.pack) pure . parseURI' <=< parseUrlPiece

instance FromJSON X509.SignedCertificate where
  parseJSON = withText "KeyInfo element" $ either fail pure . parseKeyInfo False . cs

instance ToJSON X509.SignedCertificate where
  toJSON = String . cs . renderKeyInfo

-- This can unfortunately not live in wire-api, because wire-api depends on
-- saml2-web-sso.
instance ToSchema URI where
  schema = uriToText Schema..= schema @Text `withParser` parseSchemaURI
    where
      uriToText :: URI -> Text
      uriToText = Text.decodeUtf8 . toStrict . toLazyByteString . serializeURIRef

      parseSchemaURI :: Text -> A.Parser URI
      parseSchemaURI uriText =
        either
          (\e -> fail ("Failed to parse URI " ++ Text.unpack uriText ++ " Error: " ++ show e))
          pure
          $ (parseURI strictURIParserOptions . Text.encodeUtf8) uriText

instance ToSchema Level where
  schema = assert exhaustive $ enum @Text "Level" $ mconcat $ el <$> [minBound ..]
    where
      el l = element (Text.pack (show l)) l

      exhaustive :: Bool
      exhaustive = [minBound ..] == [Trace, Debug, Info, Warn, System.Logger.Error, Fatal]

deriving instance Enum Level

deriving instance Bounded Level
