module API.User.Property (tests) where

import Imports
import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Options
import Brig.Types
import Data.Aeson
import Data.String.Conversions (cs)
import Test.Tasty hiding (Timeout)
import Util

import qualified Brig.Options                as Opt
import qualified Data.ByteString.Char8       as C
import qualified Data.Text                   as T
import qualified Network.Wai.Utilities.Error as Error

tests :: ConnectionLimit -> Opt.Timeout -> Maybe Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at opts p b _c _g = testGroup "property"
    [ test p "put/get /properties/:key - 200" $ testSetGetProperty b
    , test p "delete /properties/:key - 200"  $ testDeleteProperty b
    , test p "get /properties - 200"          $ testListPropertyKeys b
    , test p "get /properties-values - 200"   $ testListPropertyKeysAndValues b
    , test p "delete /properties - 200"       $ testClearProperties b
    , test p "put /properties/:key - 403"     $ testPropertyLimits b
    , test p "size limits"                    $ testSizeLimits opts b
    ]

testSetGetProperty :: Brig -> Http ()
testSetGetProperty brig = do
    u <- randomUser brig
    setProperty brig (userId u) "foo" objectProp !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!! do
        const 200               === statusCode
        const (Just objectProp) === responseJsonMaybe
    -- String Literals
    setProperty brig (userId u) "foo" (String "foo") !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!! do
        const 200              === statusCode
        const (Just "\"foo\"") === responseBody
    -- Boolean Literals
    setProperty brig (userId u) "foo" (Bool True) !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!! do
        const 200           === statusCode
        const (Just "true") === responseBody
    -- Numeric Literals
    setProperty brig (userId u) "foo" (Number 42) !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!! do
        const 200         === statusCode
        const (Just "42") === responseBody
  where
    objectProp = object
        [ "key.1" .= ("val1" :: Text)
        , "key.2" .= ("val2" :: Text)
        ]

testDeleteProperty :: Brig -> Http ()
testDeleteProperty brig = do
    u <- randomUser brig
    setProperty brig (userId u) "foo" (Bool True) !!!
        const 200 === statusCode
    deleteProperty brig (userId u) "foo" !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!!
        const 404 === statusCode

testListPropertyKeys :: Brig -> Http ()
testListPropertyKeys = testListProperties' "/properties"
    (toJSON ["bar" :: Text, "foo"])

testListPropertyKeysAndValues :: Brig -> Http ()
testListPropertyKeysAndValues = testListProperties' "/properties-values"
    (object ["bar" .= String "hello", "foo" .= True])

testListProperties' :: ByteString -> Value -> Brig -> Http ()
testListProperties' endpoint rval brig = do
    u <- randomUser brig
    setProperty brig (userId u) "foo" (Bool True) !!!
        const 200 === statusCode
    setProperty brig (userId u) "bar" (String "hello") !!!
        const 200 === statusCode
    get (brig . path endpoint . zUser (userId u)) !!! do
        const 200         === statusCode
        const (Just rval) === responseJsonMaybe

testClearProperties :: Brig -> Http ()
testClearProperties brig = do
    u <- randomUser brig
    setProperty brig (userId u) "foo" (Bool True) !!!
        const 200 === statusCode
    setProperty brig (userId u) "bar" (String "hello") !!!
        const 200 === statusCode
    delete (brig . path "/properties" . zUser (userId u) . zConn "conn") !!!
        const 200 === statusCode
    getProperty brig (userId u) "foo" !!!
        const 404 === statusCode
    getProperty brig (userId u) "bar" !!!
        const 404 === statusCode

testPropertyLimits :: Brig -> Http ()
testPropertyLimits brig = do
    u <- randomUser brig
    -- Maximum key length
    setProperty brig (userId u) (C.replicate 257 'x') (String "y") !!! do
        const 403 === statusCode
        const (Just "property-key-too-large") === fmap Error.label . responseJsonMaybe

    -- Maximum value length
    setProperty brig (userId u) "foo" (String (T.replicate 513 "x")) !!! do
        const 403 === statusCode
        const (Just "property-value-too-large") === fmap Error.label . responseJsonMaybe

    -- Maximum count
    forM_ [1..16 :: Int] $ \i ->
        setProperty brig (userId u) ("foo" <> C.pack (show i)) (Number (fromIntegral i)) !!!
            const 200 === statusCode
    setProperty brig (userId u) "bar" (String "hello") !!! do
        const 403 === statusCode
        const (Just "too-many-properties") === fmap Error.label . responseJsonMaybe

testSizeLimits :: HasCallStack => Maybe Opt.Opts -> Brig -> Http ()
testSizeLimits Nothing _ = error "no config!"
testSizeLimits (Just opts) brig = do
    let maxKeyLen = fromIntegral $ fromMaybe defMaxKeyLen . setPropertyMaxKeyLen $ optSettings opts
        maxValueLen = fromIntegral $ fromMaybe defMaxValueLen . setPropertyMaxValueLen $ optSettings opts

        badKey = cs $ replicate (maxKeyLen + 2) '_'
        okKey = cs $ replicate (maxKeyLen - 2) '_'

        -- we use String Values here that have an encoding that is 2 characters longer than
        -- the decoded string value (because of the quotes).
        badValue = String . cs $ replicate maxValueLen '_'
        okValue = String . cs $ replicate (maxValueLen - 3) '_'

    u <- randomUser brig
    setProperty brig (userId u) okKey okValue !!!
        const 200 === statusCode
    setProperty brig (userId u) badKey okValue !!!
        const 403 === statusCode
    setProperty brig (userId u) okKey badValue !!!
        const 403 === statusCode
    setProperty brig (userId u) badKey badValue !!!
        const 403 === statusCode
