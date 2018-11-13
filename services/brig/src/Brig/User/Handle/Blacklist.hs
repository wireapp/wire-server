{-# LANGUAGE OverloadedStrings #-}

module Brig.User.Handle.Blacklist (isBlacklistedHandle) where

import Imports
import Brig.Types.Common (Handle (..))

import qualified Data.HashSet as HashSet

-- | A blacklisted handle cannot be chosen by a (regular) user.
isBlacklistedHandle :: Handle -> Bool
isBlacklistedHandle = (`HashSet.member` blacklist)

blacklist :: HashSet Handle
blacklist = HashSet.fromList $ map Handle
    [ "account"
    , "admin"
    , "administrator"
    , "all"
    , "android"
    , "anna"
    , "avs"
    , "backend"
    , "bot"
    , "cs"
    , "design"
    , "dev"
    , "developer"
    , "development"
    , "everyone"
    , "help"
    , "helpdesk"
    , "hr"
    , "info"
    , "ios"
    , "legal"
    , "management"
    , "news"
    , "otto"
    , "payment"
    , "product"
    , "purchase"
    , "qa"
    , "support"
    , "team"
    , "user"
    , "web"
    , "wire"
    , "wirebot"
    , "wireteam"
    ]
