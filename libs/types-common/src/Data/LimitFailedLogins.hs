{-# LANGUAGE DeriveGeneric #-}

module Data.LimitFailedLogins where

import Data.Aeson
import Data.Schema as Schema
import GHC.Generics
import Wire.Arbitrary
import Wire.Data.Timeout
import Prelude

-- | Login retry limit.  In contrast to 'setUserCookieThrottle', this is not about mitigating
-- DOS attacks, but about preventing dictionary attacks.  This introduces the orthogonal risk
-- of an attacker blocking legitimate login attempts of a user by constantly keeping the retry
-- limit for that user exhausted with failed login attempts.
--
-- If in doubt, do not ues retry options and worry about encouraging / enforcing a good
-- password policy.
data LimitFailedLogins = LimitFailedLogins
  { -- | Time the user is blocked when retry limit is reached (in
    -- seconds mostly for making it easier to write a fast-ish
    -- integration test.)
    timeout :: !Timeout,
    -- | Maximum number of failed login attempts for one user.
    retryLimit :: !Int
  }
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LimitFailedLogins)

instance FromJSON LimitFailedLogins

instance ToSchema LimitFailedLogins where
  schema =
    Schema.object "LimitFailedLogins" $
      LimitFailedLogins
        <$> timeout Schema..= Schema.field "timeout" Schema.schema
        <*> retryLimit Schema..= Schema.field "retryLimit" Schema.schema
