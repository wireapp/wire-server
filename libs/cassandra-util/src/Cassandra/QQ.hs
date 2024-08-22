{-# LANGUAGE TemplateHaskellQuotes #-}

module Cassandra.QQ (sql) where

import Imports
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

-- | a simple quasi quoter to allow for tree-sitter syntax highlight injection.
-- This uses the name sql because that is known to tree-sitter, in contras to the name cql
sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quotePat = error "Cassandra.QQ: sql quasiquoter cannot be used as pattern",
      quoteType = error "Cassandra.QQ: sql quasiquoter cannot be used as type",
      quoteDec = error "Cassandra.QQ: sql quasiquoter cannot be used as declaration",
      quoteExp = appE [|fromString|] . stringE
    }
