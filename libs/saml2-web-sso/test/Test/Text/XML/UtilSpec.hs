{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Text.XML.UtilSpec
  ( spec,
  )
where

import Control.Monad ((>=>))
import SAML2.WebSSO.Test.Arbitrary ()
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck
import Text.Show.Pretty
import Text.XML
import Text.XML.Util

spec :: Spec
spec = do
  describe "hxtToConduit, conduitToHxt" $ do
    modifyMaxSize (* 2 {- it took @*10@ here to find a bug once -}) $
      it "roundtrip" . property $ \(normalizeDoc -> doc) -> do
        let msg =
              ppShow
                ( '1',
                  doc,
                  '2',
                  renderLBS def doc,
                  '3',
                  conduitToHxt @(Either String) doc,
                  '4',
                  ourDocToXMLWithRoot <$> (conduitToHxt @(Either String) doc),
                  '5',
                  (conduitToHxt >=> hxtToConduit @(Either String)) doc
                )
        counterexample msg $
          (conduitToHxt >=> fmap normalizeDoc . hxtToConduit) doc === Right doc
