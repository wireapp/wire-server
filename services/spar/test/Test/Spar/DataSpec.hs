{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.Spar.DataSpec where

import Data.Maybe
import Data.Time
import Spar.Data
import Spar.Options
import Spar.Types
import Test.Hspec
import URI.ByteString.QQ
import SAML2.WebSSO (Time(Time), addTime)


spec :: Spec
spec = do
  describe "mkTTL" $ do
    check 1 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:15Z" (Right 15)
    check 2 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:40Z" (Left TTLTooLong)
    check 3 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:00Z" (Left TTLNegative)
    check 4 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T08:30:00Z" (Left TTLNegative)

  describe "ttlToNominalDiffTime" $ do
    it "" $ do
      addTime (ttlToNominalDiffTime $ TTL 3) (Time $ parsetm "1924-07-14T08:30:00Z")
        `shouldBe` (Time $ parsetm "1924-07-14T08:30:03Z")


check :: HasCallStack => Int -> Env -> String -> Either TTLError (TTL "authresp") -> Spec
check testnumber env (parsetm -> endOfLife) expectttl =
  it (show testnumber) $ mkTTLAssertions env endOfLife `shouldBe` expectttl

mkDataEnv :: HasCallStack => String -> (TTL "authresp") -> Env
mkDataEnv now maxttl =
    Env (parsetm now)
        (SPInfo [uri|https://wire.com|] [uri|https://wire.com|])
        0      -- will not be looked at
        maxttl -- this one will

parsetm :: HasCallStack => String -> UTCTime
parsetm = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
