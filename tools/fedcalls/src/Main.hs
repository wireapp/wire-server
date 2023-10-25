{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Main
  ( main,
  )
where

import Control.Exception (assert)
import Control.Lens
import Control.Monad.State (evalState)
import Data.Data (Proxy (Proxy))
import Data.HashMap.Strict.InsOrd qualified as HM
import Data.HashSet.InsOrd (InsOrdHashSet)
import Data.Map qualified as M
import Data.OpenApi hiding (name)
import Data.OpenApi.Lens qualified as S
import Data.Text qualified as T
import Imports
import Language.Dot as D
import Servant.API
import Wire.API.MakesFederatedCall (Calls (..), FedCallFrom' (..), HasFeds (..))
import Wire.API.Routes.API
import Wire.API.Routes.Internal.Brig qualified as BrigIRoutes
import Wire.API.Routes.Public.Brig
import Wire.API.Routes.Public.Cannon
import Wire.API.Routes.Public.Cargohold
import Wire.API.Routes.Public.Galley
import Wire.API.Routes.Public.Gundeck
import Wire.API.Routes.Public.Proxy
import Wire.API.Routes.Public.Spar
import Wire.API.Routes.Version

------------------------------

main :: IO ()
main = do
  writeFile "wire-fedcalls.dot" . D.renderDot . mkDotGraph $ calls
  writeFile "wire-fedcalls.csv" . toCsv $ calls

calls :: [MakesCallTo]
calls = assert (calls' == nub calls') calls'
  where
    calls' = parse $ Proxy @Swaggers

type Swaggers =
  SpecialisedAPIRoutes 'V5 BrigAPITag
    :<|> SpecialisedAPIRoutes 'V5 CannonAPITag
    :<|> SpecialisedAPIRoutes 'V5 CargoholdAPITag
    :<|> SpecialisedAPIRoutes 'V5 GalleyAPITag
    :<|> SpecialisedAPIRoutes 'V5 GundeckAPITag
    :<|> SpecialisedAPIRoutes 'V5 ProxyAPITag
    :<|> SpecialisedAPIRoutes 'V5 SparAPITag

swaggers :: [OpenApi]
swaggers =
  [ -- TODO: introduce allSwaggerDocs in wire-api that collects these for all
    -- services, use that in /services/brig/src/Brig/API/Public.hs instead of
    -- doing it by hand.

    serviceSwagger @BrigAPITag @'V5,
    serviceSwagger @CannonAPITag @'V5,
    serviceSwagger @CargoholdAPITag @'V5,
    serviceSwagger @GalleyAPITag @'V5,
    serviceSwagger @GundeckAPITag @'V5,
    serviceSwagger @ProxyAPITag @'V5,
    serviceSwagger @SparAPITag @'V5,
    -- TODO: collect all internal apis somewhere else (brig?), and expose them
    -- via an internal swagger api end-point.

    BrigIRoutes.swaggerDoc
    -- CannonIRoutes.swaggerDoc,
    -- CargoholdIRoutes.swaggerDoc,
    -- LegalHoldIRoutes.swaggerDoc
  ]

------------------------------

data MakesCallTo = MakesCallTo
  { -- who is calling?
    sourcePath :: String,
    sourceMethod :: String,
    -- where does the call go?
    targetComp :: String,
    targetName :: String
  }
  deriving (Eq, Show)

------------------------------

fromFedCall :: FedCallFrom' Identity -> [MakesCallTo]
fromFedCall FedCallFrom {..} = do
  (comp, names) <- M.assocs $ unCalls fedCalls
  MakesCallTo
    (runIdentity name)
    (runIdentity method)
    comp
    <$> names

filterCalls :: FedCallFrom' Maybe -> Maybe (FedCallFrom' Identity)
filterCalls fedCall =
  FedCallFrom
    <$> fmap pure (name fedCall)
    <*> fmap pure (method fedCall)
    <*> pure (fedCalls fedCall)

parse :: HasFeds api => Proxy api -> [MakesCallTo]
parse p = do
  fedCallM <- evalState (getFedCalls p) mempty
  fedCallI <- maybeToList $ filterCalls fedCallM
  fromFedCall fedCallI

parse' :: OpenApi -> [MakesCallTo]
parse' oapi =
  mconcat
    . fmap (parseOperationExtensions allTags)
    . mconcat
    . fmap flattenPathItems
    . HM.toList
    $ oapi ^. S.paths
  where
    allTags = oapi ^. S.tags

-- Simple aliases to help track which field is what
type RPC = String

type Component = String

-- | extract path, method, and operation extensions
flattenPathItems :: (FilePath, PathItem) -> [((FilePath, String), InsOrdHashSet TagName)]
flattenPathItems (path, item) =
  filter ((/= mempty) . snd) $
    catMaybes
      [ ((path, "get"),) . view S.tags <$> _pathItemGet item,
        ((path, "put"),) . view S.tags <$> _pathItemPut item,
        ((path, "post"),) . view S.tags <$> _pathItemPost item,
        ((path, "delete"),) . view S.tags <$> _pathItemDelete item,
        ((path, "options"),) . view S.tags <$> _pathItemOptions item,
        ((path, "head"),) . view S.tags <$> _pathItemHead item,
        ((path, "patch"),) . view S.tags <$> _pathItemPatch item
      ]

parseOperationExtensions :: InsOrdHashSet Tag -> ((FilePath, String), InsOrdHashSet TagName) -> [MakesCallTo]
parseOperationExtensions allTags ((path, method), hm) =
  uncurry (MakesCallTo path method) <$> findCallsFedInfo allTags hm

-- Given a set of tags, and a set of tag names for an operation,
-- parse out the RPC calls and their components
findCallsFedInfo :: InsOrdHashSet Tag -> InsOrdHashSet TagName -> [(Component, RPC)]
findCallsFedInfo allTags = mapMaybe extractStrings . toList
  where
    magicPrefix :: Text
    magicPrefix = "wire-makes-federated-call-to-"
    extractStrings :: TagName -> Maybe (Component, RPC)
    extractStrings tagName =
      tag >>= \t ->
        (,)
          -- Extract the name and description, and drop everything that is empty
          -- This gives us the component name, and as a route may call the same component
          -- multiple times, it has to go into the description so it isn't dropped by the set.
          <$> fmap T.unpack t._tagDescription
          -- Strip off the magic string from the tag names, and drop empty results
          -- This also implicitly filters for results that start with the prefix.
          -- This gives us the RPC name, as that will be unique for each route, and it
          -- doesn't matter if it is set multiple times and dropped in the set, as it
          -- still describes that Fed call is made.
          <*> fmap T.unpack (T.stripPrefix magicPrefix t._tagName)
      where
        tag = find (\t -> t._tagName == tagName) allTags

------------------------------

-- | (this function can be simplified by tossing the serial numbers for nodes, but they might
-- be useful for fine-tuning the output or rendering later.)
--
-- the layout isn't very useful on realistic data sets.  maybe we can tweak it with
-- [layers](https://www.graphviz.org/docs/attr-types/layerRange/)?
mkDotGraph :: [MakesCallTo] -> D.Graph
mkDotGraph inbound = Graph StrictGraph DirectedGraph Nothing (mods <> nodes <> edges)
  where
    mods =
      [ AttributeStatement GraphAttributeStatement [AttributeSetValue (NameId "rankdir") (NameId "LR")],
        AttributeStatement NodeAttributeStatement [AttributeSetValue (NameId "shape") (NameId "rectangle")],
        AttributeStatement EdgeAttributeStatement [AttributeSetValue (NameId "style") (NameId "dashed")]
      ]
    nodes =
      [ SubgraphStatement (NewSubgraph Nothing (mkCallingNode <$> M.toList callingNodes)),
        SubgraphStatement (NewSubgraph Nothing (mkCalledNode <$> M.toList calledNodes))
      ]
    edges = mkEdge <$> inbound

    itemSourceNode :: MakesCallTo -> String
    itemSourceNode (MakesCallTo path method _ _) = method <> " " <> path

    itemTargetNode :: MakesCallTo -> String
    itemTargetNode (MakesCallTo _ _ comp rpcName) = "[" <> comp <> "]:" <> rpcName

    callingNodes :: Map String Integer
    callingNodes =
      foldl
        (\mp (i, caller) -> M.insert caller i mp)
        mempty
        ((zip [0 ..] . nub $ itemSourceNode <$> inbound) :: [(Integer, String)])

    calledNodes :: Map String Integer
    calledNodes =
      foldl
        (\mp (i, called) -> M.insert called i mp)
        mempty
        ((zip [(fromIntegral $ M.size callingNodes) ..] . nub $ itemTargetNode <$> inbound) :: [(Integer, String)])

    mkCallingNode :: (String, Integer) -> Statement
    mkCallingNode n =
      NodeStatement (mkCallingNodeId n) []

    mkCallingNodeId :: (String, Integer) -> NodeId
    mkCallingNodeId (caller, i) =
      NodeId (NameId . show $ show i <> ": " <> caller) (Just (PortC CompassW))

    mkCalledNode :: (String, Integer) -> Statement
    mkCalledNode n =
      NodeStatement (mkCalledNodeId n) []

    mkCalledNodeId :: (String, Integer) -> NodeId
    mkCalledNodeId (callee, i) =
      NodeId (NameId . show $ show i <> ": " <> callee) (Just (PortC CompassE))

    mkEdge :: MakesCallTo -> Statement
    mkEdge item =
      EdgeStatement
        [ ENodeId NoEdge (mkCallingNodeId (caller, callerId)),
          ENodeId DirectedEdge (mkCalledNodeId (callee, calleeId))
        ]
        []
      where
        caller = itemSourceNode item
        callee = itemTargetNode item
        callerId = fromMaybe (error "impossible") $ M.lookup caller callingNodes
        calleeId = fromMaybe (error "impossible") $ M.lookup callee calledNodes

------------------------------

toCsv :: [MakesCallTo] -> String
toCsv =
  intercalate "\n"
    . fmap (intercalate ",")
    . addhdr
    . fmap dolines
  where
    addhdr :: [[String]] -> [[String]]
    addhdr = (["source method", "source path", "target component", "target name"] :)

    dolines :: MakesCallTo -> [String]
    dolines (MakesCallTo spath smeth tcomp tname) = [smeth, spath, tcomp, tname]
