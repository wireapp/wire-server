{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.Spar.APISpec where

import Data.Proxy
import Arbitrary ()
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Test.Hspec

spec :: Spec
spec = do
  validateEveryToJSON (Proxy :: Proxy API.APIMeta)
  validateEveryToJSON (Proxy :: Proxy API.APIAuthReq)
  validateEveryToJSON (Proxy :: Proxy API.APIAuthResp)
  validateEveryToJSON (Proxy :: Proxy API.IdpGet)
  validateEveryToJSON (Proxy :: Proxy API.IdpGetAll)
  validateEveryToJSON (Proxy :: Proxy API.IdpCreate)
  validateEveryToJSON (Proxy :: Proxy API.IdpDelete)
