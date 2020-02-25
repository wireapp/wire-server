{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Federator.API where

import Brig.Types.Client.Prekey
import Brig.Types.Test.Arbitrary ()
import Data.Aeson.TH (deriveJSON)
import Data.FullyQualified
import Data.Handle (Handle (..))
import Data.Id (UserId)
import Federator.Util
import Imports
import Servant.API
import Servant.API.Generic
import Test.QuickCheck

data API route
  = API
      { _gapiSearch ::
          route
            :- "i"
            :> "search"
            -- QUESTION: what exactly should the query be? text + domain?
            :> QueryParam' [Required, Strict] "q" (FullyQualified Handle)
            :> Get '[JSON] FUser,
        _gapiPrekeys ::
          route
            :- "i"
            :> "users"
            :> Capture "fqu" (FullyQualified UserId)
            :> "prekeys"
            :> Get '[JSON] PrekeyBundle
      }
  deriving (Generic)

-- curl http://localhost:8097/i/search?q=wef@a.com; curl http://localhost:8097/i/users/`uuid`@example.com/prekeys

----------------------------------------------------------------------
-- TODO: add roundtrip tests for *HttpApiData, *JSON, ...
--
-- TODO: the client ids in the 'PrekeyBundle' aren't really needed here.  do we want to make a
-- new type for that, then?

data FUser
  = FUser
      { _fuGlobalHandle :: FullyQualified Handle,
        _fuFQU :: FullyQualified UserId
      }
  deriving (Eq, Show, Generic)

deriveJSON (wireJsonOptions "_fu") ''FUser

instance Arbitrary FUser where
  arbitrary = FUser <$> arbitrary <*> arbitrary

----------------------------------------------------------------------
-- ORPHANS

instance Arbitrary PrekeyBundle where
  arbitrary = PrekeyBundle <$> arbitrary <*> arbitrary

instance Arbitrary ClientPrekey where
  arbitrary = ClientPrekey <$> arbitrary <*> arbitrary
