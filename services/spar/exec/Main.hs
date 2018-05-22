module Main where

import Spar.API
import Spar.Options
import Util.Options

main :: IO ()
main = do
  -- TODO: why do we have withOpenSSL in the services?  shouldn't ssl be solely handled by nginz?

  let desc = "Spar - SSO Service"
      defaultPath = "/etc/wire/spar/conf/spar.yaml"
  options <- getOptions desc cliOptsParser defaultPath
  runServer options
