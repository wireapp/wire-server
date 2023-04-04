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

module Wire.API.MLS.CommitBundle (CommitBundle (..)) where

import Control.Applicative
import qualified Data.Swagger as S
import qualified Data.Text as T
import Imports
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

data CommitBundle = CommitBundle
  { cbCommitMsg :: RawMLS Message, -- TODO: change this type to Commit
    cbWelcome :: Maybe (RawMLS Welcome),
    cbGroupInfo :: RawMLS GroupInfo
  }
  deriving (Eq, Show)

data CommitBundleF f = CommitBundleF
  { cbCommitMsg :: f (RawMLS Message),
    cbWelcome :: f (RawMLS Welcome),
    cbGroupInfo :: f (RawMLS GroupInfo)
  }

instance Alternative f => Semigroup (CommitBundleF f) where
  cb1 <> cb2 =
    CommitBundleF
      (cb1.cbCommitMsg <|> cb2.cbCommitMsg)
      (cb1.cbWelcome <|> cb2.cbWelcome)
      (cb1.cbGroupInfo <|> cb2.cbGroupInfo)

instance Alternative f => Monoid (CommitBundleF f) where
  mempty = CommitBundleF empty empty empty

checkCommitBundleF :: CommitBundleF [] -> Either Text CommitBundle
checkCommitBundleF cb =
  CommitBundle
    <$> check "commit" cb.cbCommitMsg
    <*> checkOpt "welcome" cb.cbWelcome
    <*> check "group info" cb.cbGroupInfo
  where
    check :: Text -> [a] -> Either Text a
    check _ [x] = pure x
    check name [] = Left ("Missing " <> name)
    check name _ = Left ("Redundant occurrence of " <> name)

    checkOpt :: Text -> [a] -> Either Text (Maybe a)
    checkOpt _ [] = pure Nothing
    checkOpt _ [x] = pure (Just x)
    checkOpt name _ = Left ("Redundant occurrence of " <> name)

findMessageInStream :: Alternative f => RawMLS Message -> Either Text (CommitBundleF f)
findMessageInStream msg = case msg.rmValue.content of
  MessagePublic mp -> case mp.content.rmValue.content of
    FramedContentCommit _ -> pure (CommitBundleF (pure msg) empty empty)
    _ -> Left "unexpected public message"
  MessageWelcome w -> pure (CommitBundleF empty (pure w) empty)
  MessageGroupInfo -> error "TODO: get group info from message"
  _ -> Left "unexpected message type"

findMessagesInStream :: Alternative f => [RawMLS Message] -> Either Text (CommitBundleF f)
findMessagesInStream = getAp . foldMap (Ap . findMessageInStream)

instance ParseMLS CommitBundle where
  parseMLS = do
    msgs <- parseMLSStream parseMLS
    either (fail . T.unpack) pure $
      findMessagesInStream msgs >>= checkCommitBundleF

instance SerialiseMLS CommitBundle where
  serialiseMLS cb = do
    serialiseMLS cb.cbCommitMsg
    traverse_ serialiseMLS cb.cbWelcome
    serialiseMLS cb.cbGroupInfo

instance S.ToSchema CommitBundle where
  declareNamedSchema _ = pure (mlsSwagger "CommitBundle")
