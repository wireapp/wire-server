module Main (main) where

import OpenSSL
import qualified Gundeck.API as Api
import qualified Gundeck.Options as Options

main :: IO ()
main = withOpenSSL $ Options.parseOptions >>= Api.run
