{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.AWSSubsystem where

import Amazonka qualified as AWS
import Amazonka.SQS qualified as SQS
import Control.Monad.Catch
import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy qualified as BL
import Data.UUID hiding (null)
import Imports hiding (group)
import Polysemy (makeSem)

data AWSSubsystem m r where
  RunAwsRequest ::
    forall a m.
    ( AWS.AWSRequest a,
      Typeable a,
      Typeable (AWS.AWSResponse a)
    ) =>
    a ->
    AWSSubsystem m (Either AWS.Error (AWS.AWSResponse a))
  RunAwsRequestThrow ::
    forall a m.
    ( AWS.AWSRequest a,
      Typeable a,
      Typeable (AWS.AWSResponse a)
    ) =>
    a ->
    AWSSubsystem m (AWS.AWSResponse a)
  GetQueueUrl :: Text -> AWSSubsystem m Text
  GetJournalQueueUrl :: AWSSubsystem m (Maybe Text)
  Listen :: forall a m. (FromJSON a, Show a) => Int -> Text -> (a -> m ()) -> AWSSubsystem m ()
  EnqueueStandard :: Text -> BL.ByteString -> AWSSubsystem m SQS.SendMessageResponse
  EnqueueFIFO :: Text -> Text -> UUID -> BL.ByteString -> AWSSubsystem m SQS.SendMessageResponse

makeSem ''AWSSubsystem

data AWSSubsystemError where
  GeneralError :: (Show e, AWS.AsError e) => e -> AWSSubsystemError
  SESInvalidDomain :: AWSSubsystemError

deriving instance Show AWSSubsystemError

deriving instance Typeable AWSSubsystemError

instance Exception AWSSubsystemError
