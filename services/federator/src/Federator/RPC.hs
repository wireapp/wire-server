module Federator.RPC where

import Data.Text qualified as Text
import Imports
import Servant

newtype RPC = RPC Text

instance FromHttpApiData RPC where
  parseUrlPiece :: Text -> Either Text RPC
  parseUrlPiece rpcPath = do
    unless (Text.all isAllowedRPCChar rpcPath) $
      Left "invalid-endpoint"

    when (Text.null rpcPath) $
      Left "invalid-endpoint"
    pure $ RPC rpcPath

isAllowedRPCChar :: Char -> Bool
isAllowedRPCChar c = isAsciiLower c || isAsciiUpper c || isNumber c || c == '_' || c == '-'
