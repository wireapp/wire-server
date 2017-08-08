{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Galley.Intra.Journal
    ( teamCreate
    , teamUpdate
    , teamDelete
    , bytes
    , evData
    , nowInt
    ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Int
import Data.Foldable (for_)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Id
import Galley.Types.Teams
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX
import Galley.App
import Prelude hiding (head, mapM)
import Proto.TeamEvents

import qualified Data.UUID as UUID
import qualified Galley.Aws as Aws

-- [Note: journaling]
-- Team journal operations to SQS are a no-op when the service
-- is started without journaling arguments

teamCreate :: TeamId -> UserId -> Galley ()
teamCreate tid uid = do
    mEnv <- view aEnv
    for_ mEnv $ \e -> do
        now <- nowInt
        let event = TeamEvent TeamEvent'TEAM_CREATE (bytes tid) now (Just (evData 1 [uid]))
        Aws.execute e (Aws.enqueue event)

teamUpdate :: TeamId -> [TeamMember] -> Galley ()
teamUpdate tid mems = do
    mEnv <- view aEnv
    for_ mEnv $ \e -> do
        now <- nowInt
        let bUsers = view userId <$> filter (`hasPermission` SetBilling) mems
        let eData = evData (fromIntegral $ length mems) bUsers
        let event = TeamEvent TeamEvent'TEAM_UPDATE (bytes tid) now (Just eData)
        Aws.execute e (Aws.enqueue event)

teamDelete :: TeamId -> Galley ()
teamDelete tid = do
    mEnv <- view aEnv
    for_ mEnv $ \e -> do
        now <- nowInt
        let event = TeamEvent TeamEvent'TEAM_DELETE (bytes tid) now Nothing
        Aws.execute e (Aws.enqueue event)

----------------------------------------------------------------------------
-- utils

bytes :: Id a -> ByteString
bytes = toStrict . UUID.toByteString . toUUID

evData :: Int32 -> [UserId] -> TeamEvent'EventData
evData c uids = TeamEvent'EventData c (bytes <$> uids)

nowInt :: MonadIO m => m Int64
nowInt = liftIO $ round . utcTimeToPOSIXSeconds <$> getCurrentTime
