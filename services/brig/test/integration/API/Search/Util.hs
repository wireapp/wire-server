{-# LANGUAGE OverloadedStrings #-}

module API.Search.Util where

import Bilge
import Bilge.Assert
import Brig.Types
import Control.Monad.IO.Class
import Data.Aeson             (decode, encode)
import Data.Id
import Data.Maybe
import Data.Monoid
import Data.Text              (Text)
import Data.Text.Encoding     (encodeUtf8)
import Util
import Test.Tasty.HUnit


optIn,optOut :: SearchableStatus
optIn  = SearchableStatus True
optOut = SearchableStatus False

updateSearchableStatus :: Brig -> UserId -> SearchableStatus -> Http ()
updateSearchableStatus brig uid status =
    put ( brig
        . path "/self/searchable"
        . zUser uid
        . contentJson
        . body (RequestBodyLBS (encode status))
        ) !!!  const 200 === statusCode

executeSearch :: Brig -> UserId -> Text -> Http (Maybe (SearchResult Contact))
executeSearch brig self q = do
    r <- get ( brig
             . path "/search/contacts"
             . zUser self
             . queryItem "q" (encodeUtf8 q)
             ) <!! const 200 === statusCode
    return . decode . fromMaybe "" $ responseBody r

refreshIndex :: Brig -> Http ()
refreshIndex brig =
    post (brig . path "/i/index/refresh") !!! const 200 === statusCode

reindex :: Brig -> Http ()
reindex brig =
    post (brig . path "/i/index/reindex") !!! const 200 === statusCode

randomUserWithHandle :: Brig -> Http User
randomUserWithHandle brig = do
    u <- randomUser brig
    setRandomHandle brig u

setRandomHandle :: Brig -> User -> Http User
setRandomHandle brig user = do
    h <- randomHandle
    put ( brig
        . path "/self/handle"
        . contentJson
        . zUser (userId user)
        . zConn "c"
        . body (RequestBodyLBS . encode $ HandleUpdate h)
        ) !!!  const 200 === statusCode
    return user { userHandle = Just (Handle h) }

assertCanFind :: Brig -> UserId -> UserId -> Text -> Http ()
assertCanFind brig self expected q = do
    Just r <- (fmap . fmap) searchResults $ executeSearch brig self q
    liftIO $ do
        assertBool ("No results for query: " <> show q) $
            not (null r)
        assertBool ("User not in results for query: " <> show q) $
            elem expected . map contactUserId $ r

assertCan'tFind :: Brig -> UserId -> UserId -> Text -> Http ()
assertCan'tFind brig self expected q = do
    Just r <- (fmap . fmap) searchResults $ executeSearch brig self q
    liftIO .  assertBool ("User unexpectedly in results for query: " <> show q) $
        notElem expected . map contactUserId $ r

assertSearchable :: String -> (Request -> Request) -> UserId -> Bool -> Http ()
assertSearchable label brig uid status = do
    response <- get (brig . path "/self/searchable" . zUser uid)
    liftIO $ assertEqual (label ++ ", statuscode") 200 (statusCode response)
    liftIO $ assertEqual label (Just status) (isSearchable <$> decodeBody response)
