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

module Brig.Sem.ActivationSupply.IO (activationSupplyToIO) where

import Brig.Sem.ActivationSupply
import Brig.Types
import Data.Text
import qualified Data.Text.Ascii as Ascii
import qualified Data.Text.Encoding as T
import Imports
import OpenSSL.BN
import OpenSSL.EVP.Digest
import Polysemy
import Text.Printf

activationSupplyToIO ::
  forall r a.
  Member (Embed IO) r =>
  Sem (ActivationSupply ': r) a ->
  Sem r a
activationSupplyToIO =
  interpret $
    embed @IO . \case
      MakeActivationKey key -> mkActivationKey key
      MakeActivationCode -> mkActivationCode

mkActivationKey :: UserKey -> IO ActivationKey
mkActivationKey k = do
  d <- liftIO $ getDigestByName "SHA256"
  d' <- maybe (fail "SHA256 not found") pure d
  let bs = digestBS d' (T.encodeUtf8 $ keyText k)
  pure . ActivationKey $ Ascii.encodeBase64Url bs

mkActivationCode :: IO ActivationCode
mkActivationCode =
  ActivationCode . Ascii.unsafeFromText . pack . printf "%06d"
    <$> randIntegerZeroToNMinusOne 1000000
