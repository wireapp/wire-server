{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spar.Data where

import Cassandra
import Control.Lens ((<&>))
import Control.Monad.Identity
import Data.String.Conversions
import Data.Time
import GHC.Stack

import qualified SAML2.WebSSO as SAML


storeRequest :: (HasCallStack, MonadClient m) => SAML.ID SAML.AuthnRequest -> SAML.Time -> m ()
storeRequest (SAML.ID rid) (SAML.Time endoflife) =
    retry x5 . write ins $ params Quorum (rid, endoflife)
  where
    ins :: PrepQuery W (ST, UTCTime) ()
    ins = "INSERT INTO authreq (req, end_of_life) VALUES (?, ?)"

checkAgainstRequest :: (HasCallStack, MonadClient m) => UTCTime -> SAML.ID SAML.AuthnRequest -> m Bool
checkAgainstRequest now (SAML.ID rid) = do
    (retry x1 . query1 sel . params Quorum $ Identity rid) <&> \case
        Just (Identity (Just endoflife)) -> endoflife >= now
        _ -> False
  where
    sel :: PrepQuery R (Identity ST) (Identity (Maybe UTCTime))
    sel = "SELECT end_of_life FROM authreq WHERE req = ?"

storeAssertion :: (HasCallStack, MonadClient m) => UTCTime -> SAML.ID SAML.Assertion -> SAML.Time -> m Bool
storeAssertion now (SAML.ID aid) (SAML.Time endoflifeNew) = do
    notAReplay :: Bool <- (retry x1 . query1 sel . params Quorum $ Identity aid) <&> \case
        Just (Identity (Just endoflifeOld)) -> endoflifeOld < now
        _ -> False
    when notAReplay $ do
        retry x5 . write ins $ params Quorum (aid, endoflifeNew)
    pure notAReplay
  where
    sel :: PrepQuery R (Identity ST) (Identity (Maybe UTCTime))
    sel = "SELECT end_of_life FROM authresp WHERE resp = ?"

    ins :: PrepQuery W (ST, UTCTime) ()
    ins = "INSERT INTO authresp (resp, end_of_life) VALUES (?, ?)"


-- NOTE TO FUTURE SELF: when storing IdPs, we need to handle tenant conflicts.  we need to rely on
-- the fact that the tenant in the 'SAML.UserId' is always unique.
