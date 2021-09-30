-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.API.Connection.Remote (createConnectionToRemoteUser) where

import Brig.API.Connection.Util
import Brig.App
import qualified Brig.Data.Connection as Data
import qualified Brig.IO.Intra as Intra
import Brig.Types
import Brig.Types.User.Event
import Control.Monad.Catch (throwM)
import Data.Id as Id
import Data.Qualified
import Data.Tagged
import Imports
import qualified System.Logger.Class as Log
import System.Logger.Message
import Wire.API.Connection (RelationWithHistory (..))
import Wire.API.Federation.Error (federationNotImplemented)
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))

data ConnectionAction
  = Send
  | Resend
  | Block
  | Cancel
  | Accept

actionLogMessage :: ConnectionAction -> ByteString
actionLogMessage Send = "Creating connection request"
actionLogMessage Resend = "Resending connection request"
actionLogMessage Accept = "Accepting connection"
actionLogMessage Block = "Blocking connection"
actionLogMessage Cancel = "Cancelling connection"

logAction :: Local UserId -> Qualified UserId -> ConnectionAction -> AppIO ()
logAction self target action =
  Log.info $
    logConnection (lUnqualified self) target
      . msg (val (actionLogMessage action))

newAction :: Maybe UserConnection -> Either UserConnection ConnectionAction
newAction Nothing = Right Send
newAction (Just connection) = case ucStatus connection of
  Sent -> Right Resend
  Pending -> Right Accept
  _ -> Left connection

performAction ::
  Local UserId ->
  ConnId ->
  Remote UserId ->
  ConnectionAction ->
  AppIO (ResponseForExistedCreated UserConnection)
performAction self conn target Resend = do
  -- recreate the whole connection with a "Sent" state, to fix any possible
  -- local or remote inconsistencies
  performAction self conn target Send
performAction self conn target Send = do
  -- TODO: send RPC
  qcnv <- Intra.createConnectConv self (unTagged target) Nothing (Just conn)
  connection <- Data.insertConnection self (unTagged target) SentWithHistory qcnv
  let e2s = ConnectionUpdated connection (Just (ucStatus connection)) Nothing
  -- TODO: implement remote 1-1 conversation creation
  traverse_ (Intra.onConnectionEvent (lUnqualified self) (Just conn)) [e2s]
  pure (Created connection)
performAction _ _ _ _ = throwM federationNotImplemented

createConnectionToRemoteUser ::
  Local UserId ->
  ConnId ->
  Remote UserId ->
  ConnectionM (ResponseForExistedCreated UserConnection)
createConnectionToRemoteUser self conn target = do
  mconnection <- lift $ Data.lookupConnection self (unTagged target)
  -- the action that we take only depends on the local status of the connection
  case newAction mconnection of
    Right action -> lift $ do
      logAction self (unTagged target) action
      performAction self conn target action
    Left connection -> pure (Existed connection)
