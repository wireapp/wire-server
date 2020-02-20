{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Federator.API where

import Brig.Types.Client.Prekey
import Brig.Types.Test.Arbitrary ()
import Control.Lens ((%~), _Left)
import Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import Data.String.Conversions (cs)
import Data.UUID
import Federator.Util
import Galley.Types (EmailDomain, emailDomainText, mkEmailDomain)
import Imports
import Servant.API
import Servant.API.Generic
import Test.QuickCheck
import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as Email

data API route
  = API
      { _gapiSearch ::
          route
            :- "i" :> "search" :> QueryParam' [Required, Strict] "q" EmailAddress :> Get '[JSON] FUser,
        _gapiPrekeys ::
          route
            :- "i" :> "users" :> Capture "fqu" FQU :> "prekeys" :> Get '[JSON] PrekeyBundle
      }
  deriving (Generic)

-- curl http://localhost:8097/i/search?q=wef@a.com; curl http://localhost:8097/i/users/`uuid`@example.com/prekeys

----------------------------------------------------------------------
-- TODO: all names subject to debate.  they will go to other modules, too, but for now we'll
-- keep them all in one place here.
--
-- TODO: add roundtrip tests for *HttpApiData, *JSON, ...
--
-- TODO: the client ids in the 'PrekeyBundle' aren't really needed here.  do we want to make a
-- new type for that, then?

data FUser
  = FUser
      { _fuEmail :: EmailAddress,
        _fuFQU :: FQU
      }
  deriving (Eq, Show, Generic)

data FQU
  = FQU
      { _fquUUID :: UUID,
        _fquDomain :: EmailDomain
      }
  deriving (Eq, Show, Generic)

-- instances

instance FromHttpApiData EmailAddress where
  parseUrlPiece = (_Left %~ cs) . Email.validate . cs

instance Arbitrary EmailAddress where
  arbitrary = do
    localp <- listOf1 $ elements (['a' .. 'z'] <> ['0' .. '9'] <> ['_', '-', '+'])
    domainp <- emailDomainText <$> arbitrary
    let errmsg = error . ("arbitrary @EmailAddress: " <>)
    either errmsg pure . Email.validate $ cs localp <> "@" <> cs domainp

instance Arbitrary EmailDomain where
  arbitrary =
    either (error "arbitrary @EmailDomain") id . mkEmailDomain
      <$> elements
        [ "example.com",
          "beispiel.com"
          -- unicode domains are not supported, sadly:
          -- "例.com",
          -- "مثال.com",
          -- "dæmi.com"
        ]

instance Arbitrary FUser where
  arbitrary = FUser <$> arbitrary <*> arbitrary

instance Arbitrary FQU where
  arbitrary = FQU <$> arbitrary <*> arbitrary

deriveJSON (wireJsonOptions "_fu") ''FUser

deriveJSON (wireJsonOptions "_fqu") ''FQU

instance ToJSON EmailDomain where
  toJSON = Aeson.String . emailDomainText

instance FromJSON EmailDomain where
  parseJSON = withText "EmailDomain" $ either fail pure . mkEmailDomain . cs

instance ToJSON EmailAddress where
  toJSON = Aeson.String . cs . Email.toByteString

instance FromJSON EmailAddress where
  parseJSON = withText "EmailAddress" $ either fail pure . Email.validate . cs

instance FromHttpApiData FQU where
  parseUrlPiece raw = do
    email <- parseUrlPiece raw
    _fquDomain <- (_Left %~ cs) . mkEmailDomain . cs . Email.domainPart $ email
    _fquUUID <- maybe (fail "FQU: local part not a UUID") pure . fromText . cs . Email.localPart $ email
    pure FQU {..}

instance Arbitrary PrekeyBundle where
  arbitrary = PrekeyBundle <$> arbitrary <*> arbitrary

instance Arbitrary ClientPrekey where
  arbitrary = ClientPrekey <$> arbitrary <*> arbitrary
