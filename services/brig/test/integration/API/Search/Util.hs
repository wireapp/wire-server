module API.Search.Util where

import Bilge
import Bilge.Assert
import Brig.Types
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Fail (MonadFail)
import Data.Aeson (decode, encode)
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Imports
import Test.Tasty.HUnit
import Util

executeSearch :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> UserId -> Text -> m (Maybe (SearchResult Contact))
executeSearch brig self q = do
  r <-
    get
      ( brig
          . path "/search/contacts"
          . zUser self
          . queryItem "q" (encodeUtf8 q)
      )
      <!! const 200
      === statusCode
  return . decode . fromMaybe "" $ responseBody r

-- | ES is only refreshed occasionally; we don't want to wait for that in tests.
refreshIndex :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m ()
refreshIndex brig =
  post (brig . path "/i/index/refresh") !!! const 200 === statusCode

reindex :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> m ()
reindex brig =
  post (brig . path "/i/index/reindex") !!! const 200 === statusCode

randomUserWithHandle :: HasCallStack => Brig -> Http User
randomUserWithHandle brig = do
  u <- randomUser brig
  setRandomHandle brig u

setRandomHandle :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, HasCallStack) => Brig -> User -> m User
setRandomHandle brig user = do
  h <- randomHandle
  put
    ( brig
        . path "/self/handle"
        . contentJson
        . zUser (userId user)
        . zConn "c"
        . body (RequestBodyLBS . encode $ HandleUpdate h)
    )
    !!! const 200
    === statusCode
  return user {userHandle = Just (Handle h)}

assertCanFind :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, MonadFail m, HasCallStack) => Brig -> UserId -> UserId -> Text -> m ()
assertCanFind brig self expected q = do
  Just r <- (fmap . fmap) searchResults $ executeSearch brig self q
  liftIO $ do
    assertBool ("No results for query: " <> show q) $
      not (null r)
    assertBool ("User not in results for query: " <> show q)
      $ elem expected . map contactUserId
      $ r

assertCan'tFind :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, MonadFail m, HasCallStack) => Brig -> UserId -> UserId -> Text -> m ()
assertCan'tFind brig self expected q = do
  Just r <- (fmap . fmap) searchResults $ executeSearch brig self q
  liftIO . assertBool ("User shouldn't be present in results for query: " <> show q)
    $ notElem expected . map contactUserId
    $ r
