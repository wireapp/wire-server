{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Search.Util where

import Bilge
import Brig.Types
import Control.Monad.IO.Class
import Data.Aeson             (encode)
import Data.Id
import Data.Monoid
import Data.Text              (Text)
import GHC.Stack (HasCallStack)
import Util
import Test.Tasty.HUnit
import Network.Wire.Client.Monad
import Network.Wire.Client.Session
import Network.Wire.Client.API.Search
import Network.HTTP.Types
import Data.List.NonEmpty (NonEmpty (..))
import Named


randomUserWithHandle :: HasCallStack => Test User
randomUserWithHandle = do
    brig <- getBrig
    user <- liftHttp $ randomUser brig
    handle <- asUser (userId user) $ setRandomHandle
    pure user { userHandle = Just handle }

setRandomHandle :: HasCallStack => Test Handle
setRandomHandle = do
    h <- randomHandle
    sessionRequest Brig
        (method PUT
           . path "/self/handle"
           . contentJson
           . zConn "c"
           . body (RequestBodyLBS . encode $ HandleUpdate h))
        (status200 :| [])
        (const $ return ())
    return (Handle h)

assertCanFind :: HasCallStack => UserId -> Text -> Test ()
assertCanFind expected q = do
    r <- searchResults <$>
        search @Test ! #query q
                     ! defaults
    liftIO $ do
        assertBool ("No results for query: " <> show q) $
            not (null r)
        assertBool ("User not in results for query: " <> show q) $
            elem expected . map contactUserId $ r

assertCan'tFind :: HasCallStack => UserId -> Text -> Test ()
assertCan'tFind expected q = do
    r <- searchResults <$>
        search @Test ! #query q
                     ! defaults
    liftIO . assertBool ("User unexpectedly in results for query: " <> show q) $
        notElem expected . map contactUserId $ r

assertSearchable :: HasCallStack => String -> Bool -> Test ()
assertSearchable label expectedStatus = do
    status <- isSearchable
    liftIO $ assertEqual label (SearchableStatus expectedStatus) status
