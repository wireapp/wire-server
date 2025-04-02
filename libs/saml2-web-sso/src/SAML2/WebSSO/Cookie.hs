{-# LANGUAGE OverloadedStrings #-}

-- | A high-level wrapper for 'Web.Cookie.SetCookie' that interfaces with servant types, generates
-- and verifies cookie name from the type, handles randomness generation, and cookie deletion.
module SAML2.WebSSO.Cookie
  ( SimpleSetCookie (..),
    cookieName,
    cookieToHeader,
    toggleCookie,
    setSimpleCookieValue,
  )
where

import Control.Lens
import Control.Monad.Except
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString.Builder qualified as SBSBuilder
import Data.Proxy
import Data.String.Conversions
import Data.Text qualified as ST
import Data.Time
import GHC.TypeLits (KnownSymbol, symbolVal)
import GHC.Types
import Network.HTTP.Types.Header qualified as HttpTypes
import SAML2.WebSSO.SP
import SAML2.WebSSO.Types
import SAML2.WebSSO.XML
import Servant.API as Servant hiding (URI (..))
import Web.Cookie

newtype SimpleSetCookie name = SimpleSetCookie {fromSimpleSetCookie :: SetCookie}
  deriving (Eq, Show)

instance (KnownSymbol name) => ToHttpApiData (SimpleSetCookie name) where
  toUrlPiece = cs . SBSBuilder.toLazyByteString . renderSetCookie . fromSimpleSetCookie

instance (KnownSymbol name) => FromHttpApiData (SimpleSetCookie name) where
  parseUrlPiece = headerValueToCookie

cookieToHeader :: SimpleSetCookie name -> HttpTypes.Header
cookieToHeader =
  ("set-cookie",)
    . cs
    . toLazyByteString
    . renderSetCookie
    . fromSimpleSetCookie

cookieName :: forall (proxy :: Symbol -> Type) (name :: Symbol). (KnownSymbol name) => proxy name -> SBS
cookieName _ = cs $ symbolVal (Proxy @name)

headerValueToCookie :: forall name. (KnownSymbol name) => ST -> Either ST (SimpleSetCookie name)
headerValueToCookie txt = do
  let cookie = parseSetCookie $ cs txt
  case ["missing cookie name" | setCookieName cookie == ""]
    <> [ cs $ "wrong cookie name: got " <> setCookieName cookie <> ", expected " <> cookieName (Proxy @name)
         | setCookieName cookie /= cookieName (Proxy @name)
       ]
    <> ["missing cookie value" | setCookieValue cookie == ""] of
    errs@(_ : _) -> throwError $ ST.intercalate ", " errs
    [] -> pure (SimpleSetCookie cookie)

toggleCookie :: forall name m. (Applicative m, SP m, KnownSymbol name) => SBS -> Maybe (ST, NominalDiffTime) -> m (SimpleSetCookie name)
toggleCookie path =
  fmap SimpleSetCookie . \case
    Just (value, ttl) ->
      getNow <&> \now ->
        cookie
          { setCookieValue = cs value,
            setCookieExpires = Just . fromTime $ ttl `addTime` now,
            setCookieMaxAge = Just $ realToFrac ttl
          }
    Nothing ->
      pure
        cookie
          { setCookieValue = "",
            setCookieExpires = Just $ fromTime beginningOfTime,
            setCookieMaxAge = Just (-1)
          }
  where
    cookie =
      defaultSetCookie
        { setCookieName = cookieName (Proxy @name),
          setCookieSecure = True,
          setCookiePath = Just path,
          setCookieHttpOnly = True,
          setCookieSameSite = Just sameSiteStrict
        }

beginningOfTime :: Time
beginningOfTime = unsafeReadTime "1970-01-01T00:00:00Z"

setSimpleCookieValue :: SimpleSetCookie name -> SBS
setSimpleCookieValue = setCookieValue . fromSimpleSetCookie
