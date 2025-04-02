module Main where

import Control.Lens
import Data.String
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import Network.Wai.Utilities.Server (runSettingsWithShutdown)
import SAML2.WebSSO.API.Example (app)
import SAML2.WebSSO.Config

main :: IO ()
main = do
  putStrLn "you can find default server config files in `test/samples/server*`."
  config <- configIO
  idps <- idpConfigIO config
  let settings =
        defaultSettings
          & setHost (fromString $ config ^. cfgSPHost)
            . setPort (config ^. cfgSPPort)
  toyapp <- app config idps
  runSettingsWithShutdown settings toyapp Nothing
