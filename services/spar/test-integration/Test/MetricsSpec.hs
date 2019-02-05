{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | See also: services/brig/test/integration/API/Metrics.hs
module Test.MetricsSpec (spec) where

import Imports
import Bilge
import Control.Lens
import Data.String.Conversions (cs)
import Data.Aeson
import Data.Aeson.Lens
import Data.Set as Set
import Util


-- | path parsing works; http verb, captured variables are not distinguished in the metrics.
spec :: HasCallStack => SpecWith TestEnv
spec = describe "metrics" . it "works" $ do
  spar <- asks (^. teSpar)
  let p1 = "sso/metadata"
      p2 idpid = "sso/initiate-login/" <> idpid

  _ <- call $ get (spar . path p1)
  _ <- call $ get (spar . path (p2 "316f1c18-2980-11e9-ab0b-ef604d1791b2"))
  _ <- call $ get (spar . path (p2 "60a7dda8-2980-11e9-b359-fb5b41565453"))

  resp :: Value <- call $ jsonBody <$> get (spar . path "i/monitoring")
  let have :: Set Text = Set.fromList $ fst <$> (resp ^@.. key "net" . key "resources" . members)
      want :: Set Text = Set.fromList $ cs <$> [p1, p2 "<>"]
  liftIO $ have `shouldSatisfy` (want `Set.isSubsetOf`)

-- | Only for testing!  I've seen this in our code-base, but I can't find it any more.
jsonBody :: HasCallStack => ResponseLBS -> Value
jsonBody resp
  = either (error . show) id
  . eitherDecode
  . fromMaybe (error $ "no body: " <> show resp)
  . responseBody
  $ resp
