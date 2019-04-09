module Brig.User.Auth.Cookie
    ( -- * Cookie Essentials
      newCookie
    , newAccessToken
    , nextCookie
    , renewCookie
    , lookupCookie
    , revokeCookies
    , revokeAllCookies
    , listCookies

      -- * Limited Cookies
    , RetryAfter (..)
    , newCookieLimited

      -- * HTTP
    , setResponseCookie

      -- * Re-exports
    , Cookie (..)
    , AccessToken (..)
    , ZAuth.UserToken
    ) where

import Imports
import Brig.App
import Brig.Options hiding (user)
import Brig.Types.User.Auth hiding (user)
import Brig.User.Auth.Cookie.Limit
import Control.Lens (view)
import Data.ByteString.Conversion
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock
import Network.Wai (Response)
import Network.Wai.Utilities.Response (addHeader)
import System.Logger.Class (field, msg, val, (~~))

import qualified Brig.User.Auth.DB.Cookie as DB
import qualified Data.List                as List
import qualified System.Logger.Class      as Log
import qualified Data.Metrics             as Metrics
import qualified Web.Cookie               as WebCookie
import qualified Brig.ZAuth               as ZAuth

--------------------------------------------------------------------------------
-- Basic Cookie Management

newCookie
    :: UserId
    -> CookieType
    -> Maybe CookieLabel
    -> AppIO (Cookie ZAuth.UserToken)
newCookie u typ label = do
    now <- liftIO =<< view currentTime
    tok <- if typ == PersistentCookie
            then ZAuth.newUserToken u
            else ZAuth.newSessionToken u
    let c = Cookie
          { cookieId      = CookieId (ZAuth.userTokenRand tok)
          , cookieCreated = now
          , cookieExpires = ZAuth.tokenExpiresUTC tok
          , cookieLabel   = label
          , cookieType    = typ
          , cookieSucc    = Nothing
          , cookieValue   = tok
          }
    DB.insertCookie u c Nothing
    return c

-- | Renew the given cookie with a fresh token, if its age
-- exceeds the configured minimum threshold.
nextCookie :: Cookie ZAuth.UserToken -> AppIO (Maybe (Cookie ZAuth.UserToken))
nextCookie c = do
    s   <- view settings
    now <- liftIO =<< view currentTime
    let created  = cookieCreated c
    let renewAge = fromInteger (setUserCookieRenewAge s)
    -- TODO: Also renew the cookie if it was signed with
    -- a different zauth key index, regardless of age.
    if persist c && diffUTCTime now created > renewAge
        then Just <$> getNext
        else return Nothing
  where
    persist = (PersistentCookie ==) . cookieType
    getNext = case cookieSucc c of
        Nothing -> renewCookie c
        Just ck -> do
            let u = ZAuth.userTokenOf (cookieValue c)
            trackSuperseded u (cookieId c)
            cs <- DB.listCookies u
            case List.find (\x -> cookieId x == ck && persist x) cs of
                Nothing -> renewCookie c
                Just c' -> do
                    t <- ZAuth.mkUserToken u (cookieIdNum ck) (cookieExpires c')
                    return c' { cookieValue = t }

-- | Renew the given cookie with a fresh token.
renewCookie :: Cookie ZAuth.UserToken -> AppIO (Cookie ZAuth.UserToken)
renewCookie old = do
    let t = cookieValue old
    let u = ZAuth.userTokenOf t
    -- Insert new cookie
    new <- newCookie u (cookieType old) (cookieLabel old)
    -- Link the old cookie to the new (successor), keeping it
    -- around only for another renewal period so as not to build
    -- an ever growing chain of superseded cookies.
    let old' = old { cookieSucc = Just (cookieId new) }
    ttl <- setUserCookieRenewAge <$> view settings
    DB.insertCookie u old' (Just (DB.TTL (fromIntegral ttl)))
    return new

newAccessToken :: Cookie ZAuth.UserToken -> Maybe ZAuth.AccessToken -> AppIO AccessToken
newAccessToken c mt = do
    t' <- case mt of
       Nothing -> ZAuth.newAccessToken (cookieValue c)
       Just  t -> ZAuth.renewAccessToken t
    ttl <- view (zauthEnv.ZAuth.settings.ZAuth.accessTokenTimeout)
    return $ bearerToken (ZAuth.accessTokenOf t')
                         (toByteString t')
                         (ZAuth.accessTokenTimeoutSeconds ttl)

-- | Lookup the stored cookie associated with a user token,
-- if one exists.
lookupCookie :: ZAuth.UserToken -> AppIO (Maybe (Cookie ZAuth.UserToken))
lookupCookie t = do
    let user = ZAuth.userTokenOf t
    let rand = ZAuth.userTokenRand t
    let expi = ZAuth.tokenExpiresUTC t
    fmap setToken <$> DB.lookupCookie user expi (CookieId rand)
  where
    setToken c = c { cookieValue = t }

listCookies :: UserId -> [CookieLabel] -> AppIO [Cookie ()]
listCookies u [] = DB.listCookies u
listCookies u ll = filter byLabel <$> DB.listCookies u
  where
    byLabel c = maybe False (`elem` ll) (cookieLabel c)

revokeAllCookies :: UserId -> AppIO ()
revokeAllCookies u = revokeCookies u [] []

revokeCookies :: UserId -> [CookieId] -> [CookieLabel] -> AppIO ()
revokeCookies u [] [] = DB.deleteAllCookies u
revokeCookies u ids labels = do
    cc <- filter matching <$> DB.listCookies u
    DB.deleteCookies u cc
  where
    matching c = cookieId c `elem` ids
              || maybe False (`elem` labels) (cookieLabel c)

--------------------------------------------------------------------------------
-- Limited Cookies

newCookieLimited
    :: UserId
    -> CookieType
    -> Maybe CookieLabel
    -> AppIO (Either RetryAfter (Cookie ZAuth.UserToken))
newCookieLimited u typ label = do
    cs  <- filter ((typ ==) . cookieType) <$> DB.listCookies u
    now <- liftIO =<< view currentTime
    lim <- CookieLimit . setUserCookieLimit <$> view settings
    thr <- setUserCookieThrottle <$> view settings
    let evict = map cookieId (limitCookies lim now cs)
    if null evict
        then Right <$> newCookie u typ label
        else case throttleCookies now thr cs of
            Just wait -> return (Left wait)
            Nothing   -> do
                revokeCookies u evict []
                Right <$> newCookie u typ label

--------------------------------------------------------------------------------
-- HTTP

setResponseCookie
    :: Monad m
    => Cookie ZAuth.UserToken
    -> Response
    -> AppT m Response
setResponseCookie c r = do
    s <- view settings
    let hdr = toByteString' (WebCookie.renderSetCookie (cookie s))
    return (addHeader "Set-Cookie" hdr r)
  where
    cookie s = WebCookie.def
        { WebCookie.setCookieName     = "zuid"
        , WebCookie.setCookieValue    = toByteString' (cookieValue c)
        , WebCookie.setCookieDomain   = Just $ encodeUtf8 . setCookieDomain $ s
        , WebCookie.setCookiePath     = Just "/access"
        , WebCookie.setCookieExpires  = if cookieType c == PersistentCookie
                                            then Just (cookieExpires c)
                                            else Nothing
        , WebCookie.setCookieSecure   = not (setCookieInsecure s)
        , WebCookie.setCookieHttpOnly = True
        }

--------------------------------------------------------------------------------
-- Tracking

trackSuperseded :: UserId -> CookieId -> AppIO ()
trackSuperseded u c = do
    m <- view metrics
    Metrics.counterIncr (Metrics.path "user.auth.cookie.superseded") m
    Log.warn $ msg (val "Superseded cookie used")
        ~~ field "user" (toByteString u)
        ~~ field "cookie" (cookieIdNum c)
