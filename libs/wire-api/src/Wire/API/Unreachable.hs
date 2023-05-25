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

-- | Types and utilies around unreachable backends and failing to process
-- various kinds of messages.
module Wire.API.Unreachable
  ( -- * Failed to process
    UnreachableUsers (unreachableUsers),
    unreachableFromList,
    FailedToProcess (..),
    failedToProcessObjectSchema,
    failedToSend,
    failedToSendMaybe,
    failedToAdd,
    failedToAddMaybe,
    failedToRemove,
    failedToRemoveMaybe,
  )
where

import Control.Lens ((?~))
import qualified Data.Aeson as A
import Data.Id
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Qualified
import Data.Schema
import qualified Data.Swagger as S
import Imports

newtype UnreachableUsers = UnreachableUsers {unreachableUsers :: NonEmpty (Qualified UserId)}
  deriving stock (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema UnreachableUsers

instance Semigroup UnreachableUsers where
  (UnreachableUsers m) <> (UnreachableUsers n) = UnreachableUsers . NE.nub $ m <> n

instance ToSchema UnreachableUsers where
  schema =
    named "UnreachableUsers" $
      UnreachableUsers
        <$> unreachableUsers
          .= nonEmptyArray schema

unreachableFromList :: [Qualified UserId] -> Maybe UnreachableUsers
unreachableFromList = fmap (UnreachableUsers . NE.nub) . nonEmpty

-- | Lists of remote users that could not be processed in a federated action,
-- e.g., a message could not be sent to these remote users.
data FailedToProcess = FailedToProcess
  { send :: Maybe UnreachableUsers,
    add :: Maybe UnreachableUsers,
    remove :: Maybe UnreachableUsers
  }
  deriving (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema FailedToProcess

instance Semigroup FailedToProcess where
  ftp1 <> ftp2 =
    FailedToProcess
      { send = send ftp1 <> send ftp2,
        add = add ftp1 <> add ftp2,
        remove = remove ftp1 <> remove ftp2
      }

instance Monoid FailedToProcess where
  mempty = FailedToProcess mempty mempty mempty

failedToProcessObjectSchema :: ObjectSchema SwaggerDoc FailedToProcess
failedToProcessObjectSchema =
  FailedToProcess
    <$> send
      .= maybe_
        ( optFieldWithDocModifier
            "failed_to_send"
            (description ?~ "List of federated users who could not be reached and did not receive the message")
            (unnamed schema)
        )
    <*> add
      .= maybe_
        ( optFieldWithDocModifier
            "failed_to_add"
            (description ?~ "List of federated users who could not be reached and be added to a conversation")
            (unnamed schema)
        )
    <*> remove
      .= maybe_
        ( optFieldWithDocModifier
            "failed_to_remove"
            (description ?~ "List of federated users who could not be reached and be removed from a conversation")
            (unnamed schema)
        )

instance ToSchema FailedToProcess where
  schema = object "FailedToProcess" failedToProcessObjectSchema

failedToSend :: [Qualified UserId] -> FailedToProcess
failedToSend = failedToSendMaybe . unreachableFromList

failedToSendMaybe :: Maybe UnreachableUsers -> FailedToProcess
failedToSendMaybe us = mempty {send = us}

failedToAdd :: [Qualified UserId] -> FailedToProcess
failedToAdd = failedToAddMaybe . unreachableFromList

failedToAddMaybe :: Maybe UnreachableUsers -> FailedToProcess
failedToAddMaybe us = mempty {add = us}

failedToRemove :: [Qualified UserId] -> FailedToProcess
failedToRemove = failedToRemoveMaybe . unreachableFromList

failedToRemoveMaybe :: Maybe UnreachableUsers -> FailedToProcess
failedToRemoveMaybe us = mempty {remove = us}
