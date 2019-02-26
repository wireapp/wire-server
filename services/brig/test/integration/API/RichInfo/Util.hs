module API.RichInfo.Util where

import Imports
import Bilge
import Brig.Types
import Data.ByteString.Conversion
import Data.Id
import Util

getRichInfo
    :: HasCallStack
    => Brig
    -> UserId            -- ^ The user who is performing the query
    -> UserId            -- ^ The users whose rich info is being queried
    -> Http (Maybe RichInfo)
getRichInfo brig self uid = do
    getRichInfo_ brig self uid <&> either (const Nothing) Just

getRichInfo_
    :: HasCallStack
    => Brig
    -> UserId            -- ^ The user who is performing the query
    -> UserId            -- ^ The users whose rich info is being queried
    -> Http (Either Int RichInfo)
getRichInfo_ brig self uid = do
    r <- get ( brig
             . paths ["users", toByteString' uid, "rich-info"]
             . zUser self
             )
    if | statusCode r == 200 -> Right <$> decodeBody r
       | statusCode r `elem` [403, 404] -> pure . Left . statusCode $ r
       | otherwise -> error $
           "expected status code 200, 403, or 404, got: " <> show (statusCode r)
