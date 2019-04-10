module Database.CQL.QQ (cql) where

import Imports hiding (lift)
import qualified Data.Text as T
import Database.CQL.Parser
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

cql :: QuasiQuoter
cql =
    QuasiQuoter
    { quoteExp  = qExp
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }


qExp :: String -> Q Exp
qExp cqlStatement = do
        case parseCql (T.pack cqlStatement) of
            Left err -> fail err
            Right _  -> lift cqlStatement
