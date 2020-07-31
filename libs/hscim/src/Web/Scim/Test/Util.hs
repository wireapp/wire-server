{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Web.Scim.Test.Util
  ( shouldRespondWith,
    shouldEventuallyRespondWith,

    -- * Making wai requests
    post,
    put,
    patch,
    AcceptanceConfig (..),
    defAcceptanceConfig,
    AcceptanceQueryConfig (..),
    defAcceptanceQueryConfig,
    post',
    put',
    patch',
    get',
    delete',
    (<//>),

    -- * Request/response quasiquoter
    scim,

    -- * JSON parsing
    Field (..),
    getField,

    -- * Tag
    TestTag,
  )
where

import qualified Control.Retry as Retry
import Data.Aeson
import Data.Aeson.Internal (JSONPathElement (Key), (<?>))
import Data.Aeson.QQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as SMap
import Data.Proxy
import Data.Text
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import GHC.Stack
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Haskell.TH.Quote
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Test (SResponse)
import Test.Hspec.Expectations (expectationFailure)
import Test.Hspec.Wai hiding (patch, post, put, shouldRespondWith)
import Test.Hspec.Wai.Matcher (bodyEquals, match)
import Web.Scim.Class.Auth (AuthTypes (..))
import Web.Scim.Class.Group (GroupTypes (..))
import Web.Scim.Schema.Schema (Schema (CustomSchema, User20))
import Web.Scim.Schema.User (UserTypes (..))

-- | re-implementation of 'shouldRespondWith' with better error reporting.
-- FUTUREWORK: make this a PR upstream.  (while we're at it, we can also patch 'WaiSession'
-- and 'request' to keep track of the 'SRequest', and add that to the error message here with
-- the response.)
shouldRespondWith :: HasCallStack => WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldRespondWith action matcher =
  either (liftIO . expectationFailure) pure =<< doesRespondWith action matcher

doesRespondWith :: HasCallStack => WaiSession SResponse -> ResponseMatcher -> WaiSession (Either String ())
doesRespondWith action matcher = do
  r <- action
  let extmsg = "  details:  " <> show r <> "\n"
  pure $ maybe (Right ()) (Left . (<> extmsg)) (match r matcher)

shouldEventuallyRespondWith :: HasCallStack => WaiSession SResponse -> ResponseMatcher -> WaiExpectation
shouldEventuallyRespondWith action matcher =
  either (liftIO . expectationFailure) pure
    =<< Retry.retrying
      (Retry.exponentialBackoff 66000 <> Retry.limitRetries 6)
      (\_ -> pure . either (const True) (const False))
      (\_ -> doesRespondWith action matcher)

data AcceptanceConfig tag = AcceptanceConfig
  { scimAppAndConfig :: IO (Application, AcceptanceQueryConfig tag),
    genUserName :: IO Text,
    -- | some acceptance tests match against a fully rendered
    -- response body, which will not work when running the test
    -- as a library user (since the response will have more and
    -- other information).  if you leave this on 'False' (default
    -- from 'defAcceptanceConfig'), the test will only check some
    -- invariants on the response instead that must hold in all
    -- cases.
    responsesFullyKnown :: Bool
  }

defAcceptanceConfig :: IO Application -> AcceptanceConfig tag
defAcceptanceConfig scimApp = AcceptanceConfig {..}
  where
    scimAppAndConfig = (,defAcceptanceQueryConfig) <$> scimApp
    genUserName = ("Test_User_" <>) . UUID.toText <$> UUID.nextRandom
    responsesFullyKnown = False

data AcceptanceQueryConfig tag = AcceptanceQueryConfig
  { scimPathPrefix :: BS.ByteString,
    scimAuthToken :: BS.ByteString
  }

defAcceptanceQueryConfig :: AcceptanceQueryConfig tag
defAcceptanceQueryConfig = AcceptanceQueryConfig {..}
  where
    scimPathPrefix = ""
    scimAuthToken = "authorized"

----------------------------------------------------------------------------
-- Redefine wai test helpers to include scim+json content type

-- | avoid multiple @/@.  (kill at most one @/@ at the end of first arg and beginning of
-- second arg, resp., then add one during concatenation.
--
-- >>> ["a" <//> "b", "a" <//> "/b", "a/" <//> "b", "a/" <//> "/b"]
-- ["a/b","a/b","a/b","a/b"]
--
-- WARNING: {doctests don't work in our
-- infrastructure](https://github.com/zinfra/backend-issues/issues/1549), so this is
-- duplicated in the unit tests.
(<//>) :: ByteString -> ByteString -> ByteString
(<//>) a b = a' <> "/" <> b'
  where
    a' = maybe a (\(t, l) -> if l == '/' then t else a) $ BS8.unsnoc a
    b' = maybe b (\(h, t) -> if h == '/' then t else b) $ BS8.uncons b

post :: ByteString -> L.ByteString -> WaiSession SResponse
post path = request methodPost path [(hContentType, "application/scim+json")]

put :: ByteString -> L.ByteString -> WaiSession SResponse
put path = request methodPut path [(hContentType, "application/scim+json")]

patch :: ByteString -> L.ByteString -> WaiSession SResponse
patch path = request methodPatch path [(hContentType, "application/scim+json")]

request' :: Method -> AcceptanceQueryConfig tag -> ByteString -> L.ByteString -> WaiSession SResponse
request' method (AcceptanceQueryConfig prefix token) path = request method (prefix <//> path) [(hAuthorization, token), (hContentType, "application/scim+json")]

get' :: AcceptanceQueryConfig tag -> ByteString -> WaiSession SResponse
get' cfg path = request' methodGet cfg path ""

post' :: AcceptanceQueryConfig tag -> ByteString -> L.ByteString -> WaiSession SResponse
post' = request' methodPost

put' :: AcceptanceQueryConfig tag -> ByteString -> L.ByteString -> WaiSession SResponse
put' = request' methodPut

patch' :: AcceptanceQueryConfig tag -> ByteString -> L.ByteString -> WaiSession SResponse
patch' = request' methodPatch

delete' :: AcceptanceQueryConfig tag -> ByteString -> L.ByteString -> WaiSession SResponse
delete' = request' methodDelete

----------------------------------------------------------------------------
-- Redefine wai quasiquoter
--
-- This code was taken from Test.Hspec.Wai.JSON and modified to accept
-- @application/scim+json@. In order to keep the code simple, we also
-- require @charset=utf-8@, even though the original implementation
-- considers it optional.

-- | A response matcher and quasiquoter that should be used instead of
-- 'Test.Hspec.Wai.JSON.json'.
scim :: QuasiQuoter
scim =
  QuasiQuoter
    { quoteExp = \input -> [|fromValue $(quoteExp aesonQQ input)|],
      quotePat = const $ error "No quotePat defined for Test.Util.scim",
      quoteType = const $ error "No quoteType defined for Test.Util.scim",
      quoteDec = const $ error "No quoteDec defined for Test.Util.scim"
    }

class FromValue a where
  fromValue :: Value -> a

instance FromValue ResponseMatcher where
  fromValue = ResponseMatcher 200 [matchHeader] . equalsJSON
    where
      matchHeader = "Content-Type" <:> "application/scim+json;charset=utf-8"

equalsJSON :: Value -> MatchBody
equalsJSON expected = MatchBody matcher
  where
    matcher headers actualBody = case decode actualBody of
      Just actual | actual == expected -> Nothing
      _ -> let MatchBody m = bodyEquals (encode expected) in m headers actualBody

instance FromValue L.ByteString where
  fromValue = encode

instance FromValue Value where
  fromValue = id

----------------------------------------------------------------------------
-- Ad-hoc JSON parsing

-- | A way to parse out a single value from a JSON object by specifying the
-- field as a type-level string. Very useful when you don't want to create
-- extra types.
newtype Field (s :: Symbol) a = Field a
  deriving (Eq, Ord, Show, Read, Functor)

getField :: Field s a -> a
getField (Field a) = a

-- Copied from https://hackage.haskell.org/package/aeson-extra-0.4.1.1/docs/src/Data.Aeson.Extra.SingObject.html
instance (KnownSymbol s, FromJSON a) => FromJSON (Field s a) where
  parseJSON = withObject ("Field " <> show key) $ \obj ->
    case SMap.lookup key obj of
      Nothing -> fail $ "key " ++ show key ++ " not present"
      Just v -> Field <$> parseJSON v <?> Key key
    where
      key = pack $ symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s, ToJSON a) => ToJSON (Field s a) where
  toJSON (Field x) = object [key .= x]
    where
      key = pack $ symbolVal (Proxy :: Proxy s)

----------------------------------------------------------------------------
-- Tag

-- | A type-level tag for 'UserTypes', 'AuthTypes', etc. that allows picking any types we
-- might need in tests.
data TestTag id authData authInfo userExtra

instance UserTypes (TestTag id authData authInfo userExtra) where
  type UserId (TestTag id authData authInfo userExtra) = id
  type UserExtra (TestTag id authData authInfo userExtra) = userExtra
  supportedSchemas = [User20, CustomSchema "urn:hscim:test"]

instance GroupTypes (TestTag id authData authInfo userExtra) where
  type GroupId (TestTag id authData authInfo userExtra) = id

instance AuthTypes (TestTag id authData authInfo userExtra) where
  type AuthData (TestTag id authData authInfo userExtra) = authData
  type AuthInfo (TestTag id authData authInfo userExtra) = authInfo
