-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module Util.Activation where

import Bilge
import Control.Lens
import Data.Aeson.Lens as Aeson
import Data.ByteString.Conversion
import qualified Data.Text.Ascii as Ascii
import Imports
import Util.Types
import Wire.API.User.Activation
import Wire.API.User.Identity

getActivationCode ::
  (MonadHttp m, MonadIO m) =>
  BrigReq ->
  EmailAddress ->
  m (Maybe (ActivationKey, ActivationCode))
getActivationCode brig e = do
  let qry = queryItem "email" . toByteString' $ e
  r <-
    get
      ( brig
          . path "/i/users/activation-code"
          . qry
          . expectStatus (`elem` [200, 404])
      )
  let lbs = fromMaybe "" $ responseBody r
  let akey = ActivationKey . Ascii.unsafeFromText <$> (lbs ^? key "key" . _String)
  let acode = ActivationCode . Ascii.unsafeFromText <$> (lbs ^? key "code" . _String)
  pure $ (,) <$> akey <*> acode
