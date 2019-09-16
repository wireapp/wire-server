module Data.Metrics.Test where

import Imports

import Data.Metrics.Types
import Data.String.Conversions (cs)

import qualified Data.Text as Text
import qualified Data.Tree as Tree


-- | It is an error for one prefix to end in two different capture variables.  eg., these two
-- routes constitute a confict: "/user/:uid", "/user/:id".  There is a show instance that
-- explains this better.
data SiteConsistencyError = SiteConsistencyError
    { _siteConsistencyPrefix      :: [Text]
    , _siteConsistencyCaptureVars :: [(Text, Int)]
    }
  deriving (Eq)

instance Show SiteConsistencyError where
  show (SiteConsistencyError prefix conflicts) =
    "bad routing tables: the prefix " <>
    show ("/" <> Text.intercalate "/" prefix) <> " " <>
    "contains these variables with (very roughly) the resp. numbers of routes under them: " <>
    show conflicts

pathsConsistencyCheck :: Paths -> [SiteConsistencyError]
pathsConsistencyCheck (Paths forest) = mconcat $ go [] <$> forest
  where
    go :: [PathSegment] -> Tree.Tree PathSegment -> [SiteConsistencyError]
    go prefix (Tree.Node root trees) = maybeToList here <> (mconcat $ go (root : prefix) <$> trees)
      where
        here = findSiteConsistencyError (reverse $ root : prefix) trees

    findSiteConsistencyError :: [PathSegment] -> Tree.Forest PathSegment -> Maybe SiteConsistencyError
    findSiteConsistencyError prefix subtrees = case catMaybes $ captureVars <$> subtrees of
          []          -> Nothing
          [_]         -> Nothing
          bad@(_:_:_) -> Just $ SiteConsistencyError (either cs cs <$> prefix) bad

    captureVars :: Tree.Tree (Either ByteString any) -> Maybe (Text, Int)
    captureVars (Tree.Node (Left root) trees) = Just (cs root, weight trees)
    captureVars (Tree.Node (Right _) _) = Nothing

    weight :: Tree.Forest a -> Int
    weight = sum . fmap (length . Tree.flatten)
