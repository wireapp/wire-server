{-# LANGUAGE DeriveGeneric #-}
module Spar.Options where

import Data.Aeson
import Data.Id
import GHC.Generics
import Util.Options

import qualified SAML2.WebSSO.Config as SAML2


cliOptsParser :: Applicative m => m Opts
cliOptsParser = pure (error "spar does not support command line arguments.")
                  -- TODO: is this ok?  can i change 'getOptions' in types-common to accept a 'Maybe
                  -- Parser' then?

data Opts = Opts
    { spar          :: !Endpoint
    , saml          :: !(SAML2.Config TeamId)
    , brig          :: !Endpoint
    , cassandra     :: !CassandraOpts
    -- , optSettings   :: !Settings  -- (nothing yet; see other services for what belongs in here.)
    }
  deriving (Show, Generic)

instance FromJSON Opts
