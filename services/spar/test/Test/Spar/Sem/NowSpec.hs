{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Test.Spar.Sem.NowSpec where

import Arbitrary ()
import Data.Time
import Data.Time.Calendar.Julian
import Imports
import Polysemy
import Polysemy.Input
import SAML2.WebSSO.Types
import Spar.Sem.Now.IO
import Spar.Sem.Now.Input
import Spar.Sem.Now.Spec
import Test.Hspec
import Test.Hspec.QuickCheck

someTime :: Time
someTime = Time (UTCTime (fromJulianYearAndDay 1990 209) (secondsToDiffTime 0))

spec :: Spec
spec = do
  modifyMaxSuccess (const 1000) $ do
    propsForInterpreter "nowToIO" $ fmap Identity . runM . nowToIO . runInputConst ()
    propsForInterpreter "nowToInput" $ pure . Identity . run . runInputConst someTime . nowToInput . runInputConst ()
