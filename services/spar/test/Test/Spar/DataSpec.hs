{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.Spar.DataSpec where

import Data.Maybe
import Data.Time
import Spar.Data
import Spar.Options
import Test.Hspec


spec :: Spec
spec = do
  describe "mkTTL" $ do
    check (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:15Z" (Right 15)
    check (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:40Z" (Left TTLTooLong)
    check (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T09:00:00Z" (Left TTLNegative)
    check (mkDataEnv "1924-07-14T09:00:00Z" 30) "1924-07-14T08:30:00Z" (Left TTLNegative)


check :: HasCallStack => Env -> String -> Either TTLError (TTL "authresp") -> Spec
check env (parsetm -> endOfLife) expectttl =
  it (show (env, endOfLife)) $ mkTTLAssertions env endOfLife `shouldBe` expectttl

mkDataEnv :: HasCallStack => String -> (TTL "authresp") -> Env
mkDataEnv now maxttl = Env (parsetm now) 0{- will not be looked at -} maxttl{- this one will -}

parsetm :: HasCallStack => String -> UTCTime
parsetm = fromJust . parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
