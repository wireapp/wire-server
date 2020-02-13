module API.RichInfo.Util where

import Bilge
import Brig.Types
import Data.ByteString.Conversion
import Data.Id
import Imports
import Util

getRichInfo ::
  HasCallStack =>
  Brig ->
  -- | The user who is performing the query
  UserId ->
  -- | The users whose rich info is being queried
  UserId ->
  Http (Either Int RichInfoAssocList)
getRichInfo brig self uid = do
  r <-
    get
      ( brig
          . paths ["users", toByteString' uid, "rich-info"]
          . zUser self
      )
  if  | statusCode r == 200 -> Right <$> responseJsonError r
      | statusCode r `elem` [403, 404] -> pure . Left . statusCode $ r
      | otherwise ->
        error $
          "expected status code 200, 403, or 404, got: " <> show (statusCode r)

-- | This contacts an internal end-point.  Note the asymmetry between this and the external
-- GET end-point in the body: here we need to wrap the 'RichInfo' in a 'RichInfoUpdate'.
putRichInfo ::
  HasCallStack =>
  Brig ->
  -- | The user whose rich info is being updated
  UserId ->
  RichInfoAssocList ->
  Http ResponseLBS
putRichInfo brig uid rinfo = do
  put
    ( brig
        . paths ["i", "users", toByteString' uid, "rich-info"]
        . json (RichInfoUpdate rinfo)
    )
