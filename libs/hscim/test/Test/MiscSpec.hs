{-# LANGUAGE AllowAmbiguousTypes #-}

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

module Test.MiscSpec where

import Data.Aeson (eitherDecode', encode)
import Test.Hspec (Spec, describe, it, shouldBe)
import Web.Scim.Server.Mock (Id (..))
import Web.Scim.Test.Util ((<//>))

-- | These tests are also doctests in the prod code,
-- [but](https://github.com/zinfra/backend-issues/issues/1549).
spec :: Spec
spec = do
  describe "(<//>)" $ do
    it "works" $ do
      "a" <//> "b" `shouldBe` "a/b"
      "a" <//> "/b" `shouldBe` "a/b"
      "a/" <//> "b" `shouldBe` "a/b"
      "a/" <//> "/b" `shouldBe` "a/b"
  describe "Id" $ do
    it "works" $ do
      (eitherDecode' . encode) (Id 3) `shouldBe` Right Id {unId = 3}
