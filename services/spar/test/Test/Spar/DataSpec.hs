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

import Imports
import Data.Time
import Spar.Data
import Spar.Types
import Test.Hspec
import SAML2.WebSSO (Time(Time), addTime)


spec :: Spec
spec = do
  describe "mkTTL" $ do
    check 1 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:15Z" (Right 15)
    check 2 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:40Z" (Left (TTLTooLong "TTL:authresp:40" "TTL:authresp:30"))
    check 3 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:00Z" (Left (TTLNegative "TTL:authresp:0"))
    check 4 (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T08:30:00Z" (Left (TTLNegative "TTL:authresp:-1800"))

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
        0      -- will not be looked at
        maxttl -- this one will

parsetm :: HasCallStack => String -> UTCTime
parsetm = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
