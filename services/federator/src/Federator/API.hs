{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Federator.API where

import Brig.Types.Client.Prekey
import Brig.Types.Test.Arbitrary ()
import Data.Aeson.TH (deriveJSON)
import Data.Handle (Handle (..))
import Data.Id (ConvId, UserId)
import Data.Qualified
import Federator.Util
import Imports
import Servant.API
import Servant.API.Generic
import Test.QuickCheck (Arbitrary, arbitrary)
import qualified Wire.API.Federation.Conversation as Fed
import qualified Wire.API.Federation.Types.Event as Fed

data API route
  = API
      { _gapiSearch ::
          route
            :- "i"
            :> "search"
            -- QUESTION: what exactly should the query be? text + domain?
            :> QueryParam' [Required, Strict] "q" (Qualified Handle)
            :> Get '[JSON] FUser,
        _gapiPrekeys ::
          route
            :- "i"
            :> "users"
            :> Capture "fqu" (Qualified UserId)
            :> "prekeys"
            :> Get '[JSON] PrekeyBundle,
        _gapiJoinConversationById ::
          route
            :- "i"
            :> "conversations"
            :> Capture "cnv" (Qualified ConvId)
            :> "join"
            :> ReqBody '[JSON] JoinConversationByIdRequest
            :> Post '[JSON] (Fed.ConversationUpdateResult Fed.MemberJoin)
      }
  deriving (Generic)

-- curl http://localhost:8097/i/search?q=wef@a.com; curl http://localhost:8097/i/users/`uuid`@example.com/prekeys

----------------------------------------------------------------------
-- TODO: add roundtrip tests for *HttpApiData, *JSON, ...
--
-- TODO: the client ids in the 'PrekeyBundle' aren't really needed here.  do we want to make a
-- new type for that, then?

data JoinConversationByIdRequest
  = JoinConversationByIdRequest
      { joinUserId :: Qualified UserId
      }
  deriving (Eq, Show, Generic)

data FUser
  = FUser
      { _fuGlobalHandle :: !(Qualified Handle),
        _fuFQU :: !(Qualified UserId)
      }
  deriving (Eq, Show, Generic)

-- TODO: use Generics
deriveJSON wireJsonOptions ''JoinConversationByIdRequest

deriveJSON wireJsonOptions ''FUser

instance Arbitrary FUser where
  arbitrary = FUser <$> arbitrary <*> arbitrary

----------------------------------------------------------------------
-- ORPHANS

instance Arbitrary PrekeyBundle where
  arbitrary = PrekeyBundle <$> arbitrary <*> arbitrary

instance Arbitrary ClientPrekey where
  arbitrary = ClientPrekey <$> arbitrary <*> arbitrary
