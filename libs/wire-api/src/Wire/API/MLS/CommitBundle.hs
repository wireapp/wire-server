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
import Data.OpenApi qualified as S
import Data.Text qualified as T
import Imports
import Wire.API.MLS.GroupInfo
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

data CommitBundle = CommitBundle
  { commitMsg :: RawMLS Message,
    welcome :: Maybe (RawMLS Welcome),
    groupInfo :: RawMLS GroupInfo,
    appMessage :: Maybe (RawMLS Message)
  }
  deriving stock (Eq, Show, Generic)

data CommitBundleF f = CommitBundleF
  { commitMsg :: f (RawMLS Message),
    welcome :: f (RawMLS Welcome),
    groupInfo :: f (RawMLS GroupInfo),
    appMessage :: f (RawMLS Message)
  }

deriving instance Show (CommitBundleF [])

instance (Alternative f) => Semigroup (CommitBundleF f) where
  cb1 <> cb2 =
    CommitBundleF
      (cb1.commitMsg <|> cb2.commitMsg)
      (cb1.welcome <|> cb2.welcome)
      (cb1.groupInfo <|> cb2.groupInfo)
      (cb1.appMessage <|> cb2.appMessage)

instance (Alternative f) => Monoid (CommitBundleF f) where
  mempty = CommitBundleF empty empty empty empty

checkCommitBundleF :: CommitBundleF [] -> Either Text CommitBundle
checkCommitBundleF cb =
  CommitBundle
    <$> check "commit" cb.commitMsg
    <*> checkOpt "welcome" cb.welcome
    <*> check "group info" cb.groupInfo
    <*> checkOpt "application message" cb.appMessage
  where
    check :: Text -> [a] -> Either Text a
    check _ [x] = pure x
    check name [] = Left ("Missing " <> name)
    check name _ = Left ("Redundant occurrence of " <> name)

    checkOpt :: Text -> [a] -> Either Text (Maybe a)
    checkOpt _ [] = pure Nothing
    checkOpt _ [x] = pure (Just x)
    checkOpt name _ = Left ("Redundant occurrence of " <> name)

findMessageInStream :: (Alternative f) => RawMLS Message -> Either Text (CommitBundleF f)
findMessageInStream msg = case msg.value.content of
  MessagePublic mp -> case mp.content.value.content of
    FramedContentCommit _ -> pure (CommitBundleF (pure msg) empty empty empty)
    _ -> Left "unexpected proposal"
  MessageWelcome w -> pure (CommitBundleF empty (pure w) empty empty)
  MessageGroupInfo gi -> pure (CommitBundleF empty empty (pure gi) empty)
  MessagePrivate _ -> pure (CommitBundleF empty empty empty (pure msg))
  _ -> Left "unexpected message type"

findMessagesInStream :: (Alternative f) => [RawMLS Message] -> Either Text (CommitBundleF f)
findMessagesInStream = getAp . foldMap (Ap . findMessageInStream)

instance ParseMLS CommitBundle where
  parseMLS = do
    msgs <- parseMLSStream parseMLS
    either (fail . T.unpack) pure $
      findMessagesInStream msgs >>= checkCommitBundleF

instance SerialiseMLS CommitBundle where
  serialiseMLS cb = do
    serialiseMLS cb.commitMsg
    traverse_ (serialiseMLS . mkMessage . MessageWelcome) cb.welcome
    serialiseMLS $ mkMessage (MessageGroupInfo cb.groupInfo)

instance S.ToSchema CommitBundle where
  declareNamedSchema _ = pure (mlsSwagger "CommitBundle")
