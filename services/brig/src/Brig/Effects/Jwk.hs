{-# LANGUAGE TemplateHaskell #-}

module Brig.Effects.Jwk where

import Control.Exception
import Crypto.JOSE.JWK
import Data.Aeson
import qualified Data.ByteString as BS
import Data.String.Conversions (cs)
import Imports
import Polysemy

data Jwk m a where
  Get :: FilePath -> Jwk m (Maybe JWK)

makeSem ''Jwk

interpretJwk :: Members '[Embed IO] r => Sem (Jwk ': r) a -> Sem r a
interpretJwk = interpret $ \(Get fp) -> do
  contents :: Either IOException ByteString <- liftIO $ try $ BS.readFile fp
  pure $ either (const Nothing) (decode . cs) contents

interpretFakeJwk :: Members '[Embed IO] r => Sem (Jwk ': r) a -> Sem r a
interpretFakeJwk = interpret $ \(Get _) -> pure $ fakeJwk

fakeJwk :: Maybe JWK
fakeJwk = decode "{\"p\":\"8U9gI_GHo8ca1g-6Miv9f7E7zTvY90mPZO9Hhygf3ZxhFk_TNH7y3dMyUZebnYLbXf1wUltLve-nND9AO2omtz6WgPEjPomo6nLeIO1swzkBkTqrQhvPioo7rXIAlv4O5vEPPnLFJceSVRmDSGovkjcyklAhZiVRfzAv-_GdnG8\",\"kty\":\"RSA\",\"q\":\"43dqCXHtiIYJ2bvVvVW8Ch4yhJmPA9VUQSTN2aVlLZoqMOLh8rn6fl0UiBLCQJdoPI3Hc2QuS6_GohVrKI-WgpHEGMssZnH9cbfGuNUX7QK6glhsGSPoxSRpFJMgVDxG5jCgUSOt0BJdY_hgo7wwO_bx2VTdSJPgIt761TUNjiU\",\"d\":\"ixZk7xvHUYzY8Eu0NwAF5LoGG7xJSqSuvy7lg4Ag8Pb8imlvwvyo1G2aYpNXGWi9lTv8h_tnVaSBfb7KddS9KLpoC-EuBk6tn8EUyeevKdRD6c-WLZX6QehET_B_LWL_EQQRB2cqfiThkDghN5HWZn18-QskyYgS0vWS_EYquM8LS3s0emQhXkz77ZX4fQrhZGRCAmDeI-iZDt90uRTF6OceKKoC5eTimx9YimQ0Z-qOlKiCpIjplSgHL8QgIZrp6mLdCTVy3vxaegwSNKwGqCCwcewtdBky9nb9pX6sEc6CA3WfyYZMbthMYIYTi0KJ2kaaF2QdR3l7VLTdPBwK-Q\",\"e\":\"AQAB\",\"use\":\"sig\",\"kid\":\"QVapB_JRK66AMCYKN7LHsl4DlkICTNYB6ExJrtmva88\",\"qi\":\"sYHbPsyiiRIBKbN-chbXYTLsd03jL_kiT2VdyMsP_tNjOz87WcC8Td-lBUIViw_aMq1VOjJEyhB6yE9-Po4YmlAFbPSf1rmypAdjTKRQznkuTGDue6yd4z1t4SgZJOSlpJSGBtkEiYaIlI9-fviPxtIpDh0JfAbE7XgPItu5vT8\",\"dp\":\"LFmnVNPWVx6H6yHW2dPF8osTb6P5O3HUnftwTQHt4WAVY_Fl4vcWEfj3_ZD8s9VSFhM3apuG7zC1rV-WoZTf6rvhE2--R4creXOFKc4ZOEQo6pU61pqf6VmAoF8chqPI_178q0CSxV_JoERhIZUKizgD3mpFyS1ArjygBREiI5c\",\"alg\":\"RS256\",\"dq\":\"UXTY7yryQhql-mfugc_q98CanKuU8G17r5xrbw5wriTwCPOmsFJzZr8UdCorkA_oTw8CpsrwXUBEJJUA-9R3tKhYjfxsGP-nIAsMyfHdkI2SlsztYQ9f8wE39Bs_z4qKZTJyprvdKY76DKvMh7YDm-Wx8_8VA0GZWUN6ldFa2lU\",\"n\":\"1mnyGVTdwCi4umlGMvT6y7aTmibMQuBvySoT5eZcwKti4_sfnEJhyWXfsE6tasUC8ce7YGzFiq886S9-iZc6hYW0ReIPQYCLZ5hn-fUCxefEZdX03e-uJww7OwX_kNQifgdYwzM__QJsy1_nbgEn7olTzplJfJmUSk9dkfFJ-3o5L3AJ1YMbFPgXae0OLmkxjVKfT_6093a0RLSpUCzcti4OhtNa-OEUMX4rYl0jQyzZyO281IH-MatecA1O_o6JbCI1wH46jWNY8of43cemzFwQaS2SnSyppOmJzGf9fpm5j5dozYrAdTdPYdZEA62CimvdLvJqEt4OjyQuRTAuCw\"}"
