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

module Brig.Effects.PasswordResetStore.CodeStore
  ( passwordResetStoreToCodeStore,
  )
where

import Brig.Effects.CodeStore
import Brig.Effects.PasswordResetStore
import Brig.Types.User (PasswordResetPair)
import Data.Id
import Data.Time
import Imports hiding (lookup)
import Polysemy
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.Sem.Now
import qualified Wire.Sem.Now as Now

passwordResetStoreToCodeStore ::
  forall r a.
  Members '[CodeStore, Now] r =>
  Sem (PasswordResetStore ': r) a ->
  Sem r a
passwordResetStoreToCodeStore = interpret $ \case
  CreatePasswordResetCode uid eEmailPhone -> create uid eEmailPhone
  LookupPasswordResetCode uid -> lookup uid
  VerifyPasswordResetCode prp -> verify prp

maxAttempts :: Int32
maxAttempts = 3

ttl :: NominalDiffTime
ttl = 3600 -- 60 minutes

create ::
  Members '[CodeStore, Now] r =>
  UserId ->
  Either Email Phone ->
  Sem r PasswordResetPair
create u target = do
  key <- mkPasswordResetKey u
  now <- Now.get
  code <- either (const generateEmailCode) (const generatePhoneCode) target
  codeInsert
    key
    (PRQueryData code u (Identity maxAttempts) (Identity (ttl `addUTCTime` now)))
    (round ttl)
  pure (key, code)

lookup ::
  Members '[CodeStore, Now] r =>
  UserId ->
  Sem r (Maybe PasswordResetCode)
lookup u = do
  key <- mkPasswordResetKey u
  now <- Now.get
  validate now =<< codeSelect key
  where
    validate now (Just (PRQueryData c _ _ (Just t))) | t > now = pure $ Just c
    validate _ _ = pure Nothing

verify ::
  Members '[CodeStore, Now] r =>
  PasswordResetPair ->
  Sem r (Maybe UserId)
verify (k, c) = do
  now <- Now.get
  code <- codeSelect k
  case code of
    Just (PRQueryData c' u _ (Just t)) | c == c' && t >= now -> pure (Just u)
    Just (PRQueryData c' u (Just n) (Just t)) | n > 1 && t > now -> do
      codeInsert k (PRQueryData c' u (Identity (n - 1)) (Identity t)) (round ttl)
      pure Nothing
    Just PRQueryData {} -> codeDelete k $> Nothing
    Nothing -> pure Nothing
