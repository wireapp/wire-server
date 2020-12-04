{-# LANGUAGE RecordWildCards #-}

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

module Brig.Calling
  ( Env (..),
    newEnv,
    turnServers,
    turnTokenTTL,
    turnConfigTTL,
    turnSecret,
    turnSHA512,
    turnPrng,
    randomize,
  )
where

import Brig.Types (TurnURI)
import Control.Lens
import Control.Monad.Random.Class (MonadRandom)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List1
import Imports
import OpenSSL.EVP.Digest (Digest)
import System.Random.MWC (GenIO, createSystemRandom)
import System.Random.Shuffle

-- | Note: Even though 'shuffleM' works only for [a], input is NonEmpty so it's
-- safe to NonEmpty.fromList; ideally, we'd have 'shuffleM' for 'NonEmpty'
randomize :: (MonadRandom m) => NonEmpty a -> m (NonEmpty a)
randomize xs = NonEmpty.fromList <$> shuffleM (NonEmpty.toList xs)

-- TURN specific
data Env = Env
  { _turnServers :: List1 TurnURI,
    _turnTokenTTL :: Word32,
    _turnConfigTTL :: Word32,
    _turnSecret :: ByteString,
    _turnSHA512 :: Digest,
    _turnPrng :: GenIO
  }

makeLenses ''Env

newEnv :: Digest -> List1 TurnURI -> Word32 -> Word32 -> ByteString -> IO Env
newEnv sha512 srvs tTTL cTTL secret = Env srvs tTTL cTTL secret sha512 <$> createSystemRandom
