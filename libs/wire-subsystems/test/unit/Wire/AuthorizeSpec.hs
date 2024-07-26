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

module Wire.AuthorizeSpec (spec) where

import Imports
import Test.Hspec
import Wire.Authorize

spec :: Spec
spec = describe "Wire.Authorize" do
  describe "AuthorizeUpdateEmail" do
    describe "fails if..." do
      it "no credentials at all" do
        authorize @AuthorizeUpdateEmail ([], Nothing) `shouldBe` Left undefined
      it "no cookies" do
        pending
      it "syntactically invalid session token" do
        pending
      it "syntactically invalid cookie" do
        pending
      it "expired session token" do
        pending
      it "expired cookie" do
        pending
      it "expired invalid cookie + valid cookie + valid session token" do
        -- (not sure about this one)
        pending
      it "syntactically invalid cookie + valid cookie + valid session token" do
        -- (not sure about this one)
        pending
    describe "succceeds if..." do
      it "valid cookie and valid session token" do
        pending
      it "several valid cookies and valid session token" do
        -- (not sure about this one)
        pending
