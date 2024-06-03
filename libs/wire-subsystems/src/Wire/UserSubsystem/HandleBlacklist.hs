module Wire.UserSubsystem.HandleBlacklist
  ( isBlacklistedHandle,
  )
where

import Control.Exception (assert)
import Data.Handle (Handle, parseHandle)
import Data.HashSet qualified as HashSet
import Imports

-- | A blacklisted handle cannot be chosen by a (regular) user.
isBlacklistedHandle :: Handle -> Bool
isBlacklistedHandle = (`HashSet.member` blacklist)

blacklist :: HashSet Handle
blacklist = assert good (HashSet.fromList (fromJust <$> parsed))
  where
    good = all isJust parsed
    parsed = parseHandle <$> raw
    raw =
      [ "account",
        "admin",
        "administrator",
        "all",
        "android",
        "anna",
        "avs",
        "backend",
        "bot",
        "cs",
        "design",
        "dev",
        "developer",
        "development",
        "everyone",
        "help",
        "helpdesk",
        "hr",
        "info",
        "ios",
        "legal",
        "management",
        "news",
        "otto",
        "payment",
        "product",
        "purchase",
        "qa",
        "support",
        "team",
        "user",
        "web",
        "wire",
        "wirebot",
        "wireteam"
      ]
