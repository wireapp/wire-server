{-# LANGUAGE StrictData #-}

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

-- FUTUREWORK: replace `Wire.API.User.Auth` with this module once everything in `Auth` is migrated to schema-profunctor
module Wire.API.User.Auth2 where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types as A
import Data.Handle (Handle)
import Data.Schema
import qualified Data.Swagger as S
import Data.Tuple.Extra (fst3, snd3, thd3)
import Imports
import Wire.API.User.Identity (Email, Phone)
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

--------------------------------------------------------------------------------
-- LoginId

data LoginId
  = LoginByEmail Email
  | LoginByPhone Phone
  | LoginByHandle Handle
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LoginId)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LoginId)

-- NB. this should fail if (e.g.) the email is present but unparseable even if the JSON contains a valid phone number or handle.
-- See tests in `Test.Wire.API.User.Auth`.
instance ToSchema LoginId where
  schema =
    object "LoginId" $
      fromLoginId .= tupleSchema `withParser` validate
    where
      fromLoginId :: LoginId -> (Maybe Email, Maybe Phone, Maybe Handle)
      fromLoginId = \case
        LoginByEmail e -> (Just e, Nothing, Nothing)
        LoginByPhone p -> (Nothing, Just p, Nothing)
        LoginByHandle h -> (Nothing, Nothing, Just h)
      tupleSchema :: ObjectSchema SwaggerDoc (Maybe Email, Maybe Phone, Maybe Handle)
      tupleSchema =
        (,,)
          <$> fst3 .= maybe_ (optField "email" schema)
          <*> snd3 .= maybe_ (optField "phone" schema)
          <*> thd3 .= maybe_ (optField "handle" schema)
      validate :: (Maybe Email, Maybe Phone, Maybe Handle) -> A.Parser LoginId
      validate (mEmail, mPhone, mHandle) =
        maybe (fail "'email', 'phone' or 'handle' required") pure $
          (LoginByEmail <$> mEmail) <|> (LoginByPhone <$> mPhone) <|> (LoginByHandle <$> mHandle)
