module API.BrigCommon where

import API.Common
import Data.Aeson.Types (Pair)
import Data.Maybe
import Testlib.Prelude as Prelude

data AddClient = AddClient
  { ctype :: String, -- "temporary", "permanent", "legalhold"
    internal :: Bool,
    clabel :: String,
    model :: String,
    prekeys :: Maybe [Value],
    lastPrekey :: Maybe Value,
    password :: String,
    acapabilities :: Maybe [String]
  }

instance Default AddClient where
  def =
    AddClient
      { ctype = "permanent",
        internal = False,
        clabel = "Test Device",
        model = "Test Model",
        prekeys = Nothing,
        lastPrekey = Nothing,
        password = defPassword,
        acapabilities = Just ["legalhold-implicit-consent"]
      }

mkAddClientValue :: AddClient -> App [Pair]
mkAddClientValue args = do
  pks <- maybe (fmap pure getPrekey) pure args.prekeys
  lpk <- maybe getLastPrekey pure args.lastPrekey
  pure
    [ "prekeys" .= pks,
      "lastkey" .= lpk,
      "type" .= args.ctype,
      "label" .= args.clabel,
      "model" .= args.model,
      "password" .= args.password,
      "capabilities" .= args.acapabilities
    ]
