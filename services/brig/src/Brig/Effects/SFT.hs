-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Brig.Effects.SFT where

import Data.ByteString (unsnoc)
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Conversion.From
import Data.ByteString.Internal (ByteString (PS), w2c)
import Data.ByteString.Unsafe (unsafeTake)
import Data.IP
import Data.Misc
import Data.String.Conversions (cs)
import Imports
import Network.HTTP.Client
import Polysemy
import Polysemy.Internal
import Wire.API.Call.Config

newtype SFTError = SFTError {unSFTError :: String}

data SFT m a where
  SFTGetClientUrl :: IPv4 -> Port -> SFT m (Either SFTError SFTServer)

sftGetClientUrl :: Member SFT r => IPv4 -> Port -> Sem r (Either SFTError SFTServer)
sftGetClientUrl ipAddr port = send $ SFTGetClientUrl ipAddr port

interpretSFT :: Member (Embed IO) r => Manager -> Sem (SFT ': r) a -> Sem r a
interpretSFT httpManager = interpret $ \(SFTGetClientUrl ipAddr port) -> do
  let req =
        parseRequest_ $
          mconcat
            [ "GET http://",
              show ipAddr,
              ":",
              show . portNumber $ port,
              "/sft/url"
            ]
  sftUrlResponse <- liftIO (responseBody <$> httpLbs req httpManager)
  pure . bimap SFTError sftServer . runParser' (parser @HttpsUrl) . cs . strip . cs $ sftUrlResponse
  where
    -- FUTUREWORK: remove this adopted code once upgraded to bytestring >= 0.10.12.0
    strip :: BS8.ByteString -> BS8.ByteString
    strip = BS8.dropWhile isSpace . dropWhileEnd' isSpace
      where
        dropWhileEnd' :: (Char -> Bool) -> BS8.ByteString -> BS8.ByteString
        dropWhileEnd' f ps = unsafeTake (findFromEndUntil (not . f . w2c) ps) ps
        findFromEndUntil :: (Word8 -> Bool) -> BS8.ByteString -> Int
        findFromEndUntil f ps@(PS _ _ l) = case unsnoc ps of
          Nothing -> 0
          Just (b, c) ->
            if f c
              then l
              else findFromEndUntil f b
