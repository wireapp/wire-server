module API.RichInfo.Util where

import Imports
import Bilge
import Brig.Types
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
import Util

getRichInfo
    :: HasCallStack
    => Brig
    -> UserId            -- ^ The user who is performing the query
    -> UserId            -- ^ The users whose rich info is being queried
    -> Http (Either Int RichInfo)
getRichInfo brig self uid = do
    r <- get ( brig
             . paths ["users", toByteString' uid, "rich-info"]
             . zUser self
             )
    if | statusCode r == 200 -> Right <$> decodeBody r
       | statusCode r `elem` [403, 404] -> pure . Left . statusCode $ r
       | otherwise -> error $
           "expected status code 200, 403, or 404, got: " <> show (statusCode r)

-- | This contacts an internal end-point.  Note the assymetry between this and the external
-- GET end-point in the body: here we need to wrap the 'RichInfo' encoding in another object
-- with one attribute named `rich_info`.
putRichInfo
    :: HasCallStack
    => Brig
    -> UserId            -- ^ The users whose rich info is being updated
    -> RichInfo
    -> Http ResponseLBS
putRichInfo brig uid rinfo = do
    put ( brig
        . paths ["i", "users", toByteString' uid, "rich-info"]
        . json (object ["rich_info" .= rinfo])
        )
