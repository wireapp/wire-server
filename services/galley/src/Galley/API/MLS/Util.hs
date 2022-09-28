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

module Galley.API.MLS.Util where

import Control.Comonad
import Data.Id
import Data.Qualified
import Galley.API.Util
import Galley.Data.Conversation.Types hiding (Conversation)
import qualified Galley.Data.Conversation.Types as Data
import Galley.Effects
import Galley.Effects.ConversationStore
import Galley.Effects.MemberStore
import Imports
import Polysemy
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley

getLocalConvForUser ::
  Members
    '[ ErrorS 'ConvNotFound,
       ConversationStore,
       Input (Local ()),
       MemberStore
     ]
    r =>
  Qualified UserId ->
  Local ConvId ->
  Sem r Data.Conversation
getLocalConvForUser qusr lcnv = do
  conv <- getConversation (tUnqualified lcnv) >>= noteS @'ConvNotFound

  -- check that sender is part of conversation
  loc <- qualifyLocal ()
  isMember' <- foldQualified loc (fmap isJust . getLocalMember (convId conv) . tUnqualified) (fmap isJust . getRemoteMember (convId conv)) qusr
  unless isMember' $ throwS @'ConvNotFound

  pure conv
