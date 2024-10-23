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

module Main
  ( main,
  )
where

import Imports
import Test.Data.Mailbox qualified as Mailbox
import Test.Data.PEMKeys qualified as PEMKeys
import Test.Domain qualified as Domain
import Test.Handle qualified as Handle
import Test.Properties qualified as Properties
import Test.Qualified qualified as Qualified
import Test.SizedHashMap qualified as SizedHashMap
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ Properties.tests,
        SizedHashMap.tests,
        Domain.tests,
        Handle.tests,
        Qualified.tests,
        PEMKeys.tests,
        Mailbox.tests
      ]
