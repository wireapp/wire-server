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

module Test.Spar.Intra.BrigSpec
  ( spec,
  )
where

import Brig.Types.Common (fromEmail)
import Control.Lens ((^.))
import Data.Id (Id (Id))
import qualified Data.UUID as UUID
import Imports hiding (head)
import qualified Spar.Intra.Brig as Intra
import Util
import qualified Web.Scim.Schema.User as Scim.User

spec :: SpecWith TestEnv
spec = do
  describe "user deletion between brig and spar" $ do
    it "if a user gets deleted on brig, it will be deleted on spar as well." $ do
      pending
    it "if a user gets deleted on spar, it will be deleted on brig as well." $ do
      pendingWith "or deactivated?  we should decide what we want here."

  describe "getBrigUser" $ do
    it "return Nothing if n/a" $ do
      musr <- runSpar $ Intra.getBrigUser Intra.NoPendingInvitations (Id . fromJust $ UUID.fromText "29546d9e-ed5b-11ea-8228-c324b1ea1030")
      liftIO $ musr `shouldSatisfy` isNothing

    it "return Just if /a" $ do
      let setup = do
            env <- ask
            email <- randomEmail
            scimUser <- randomScimUser <&> \u -> u {Scim.User.externalId = Just $ fromEmail email}
            (_, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
            tok <- registerScimToken tid Nothing
            scimUserId <$> createUser tok scimUser

      uid <- setup
      musr <- runSpar $ Intra.getBrigUser Intra.NoPendingInvitations uid
      liftIO $ musr `shouldSatisfy` isJust
