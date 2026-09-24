-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.CodeStore where

import Data.Code (Key)
import Data.Domain (Domain)
import Data.Map qualified as Map
import Data.Misc (HttpsUrl)
import Imports
import Polysemy
import Polysemy.Input (Input, input)
import Polysemy.State (State, gets, modify)
import Wire.API.Password (Password)
import Wire.CodeStore
import Wire.CodeStore.Code (CodeReferent (..), codeReferent)
import Wire.CodeStore.Code qualified as Code

-- | Pure in-memory 'CodeStore' for tests. Codes are keyed by their 'Key' in
-- the 'State'; the configured conversation-code URI (single-domain or
-- per-domain map) is supplied via 'Input'.
interpretCodeStorePure ::
  ( Member (State (Map Key (Code.Code, Maybe Password))) r,
    Member (Input (Either HttpsUrl (Map Domain HttpsUrl))) r,
    Member (Embed IO) r
  ) =>
  InterpreterFor CodeStore r
interpretCodeStorePure = interpret $ \case
  CreateCode code mPw -> do
    k <- embed (Code.mkKey (codeReferent code))
    modify (Map.insert k (code, mPw))
  GetCode k -> gets (Map.lookup k)
  DeleteConversationCode cid -> do
    k <- embed (Code.mkKey (CodeReferentConv cid))
    modify (Map.delete k)
  DeleteMeetingCode mid -> do
    k <- embed (Code.mkKey (CodeReferentMeeting mid))
    modify (Map.delete k)
  CreateMeetingCode mid t -> do
    code <- embed (Code.generate (CodeReferentMeeting mid) t)
    k <- embed (Code.mkKey (CodeReferentMeeting mid))
    modify (Map.insert k (code, Nothing))
    pure True
  MakeKey ref -> embed (Code.mkKey ref)
  GenerateCode ref t -> embed (Code.generate ref t)
  GetConversationCodeURI mbHost -> do
    convCodeURI <- input
    pure $ case convCodeURI of
      Left uri -> Just uri
      Right m -> mbHost >>= flip Map.lookup m
