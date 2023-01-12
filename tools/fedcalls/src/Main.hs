{-# LANGUAGE OverloadedStrings #-}

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
import Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict.InsOrd as HM
import qualified Data.Map as M
import Data.Swagger
  ( PathItem,
    Swagger,
    _operationExtensions,
    _pathItemDelete,
    _pathItemGet,
    _pathItemHead,
    _pathItemOptions,
    _pathItemPatch,
    _pathItemPost,
    _pathItemPut,
    _swaggerPaths,
  )
import Imports
import Language.Dot as D
import qualified Wire.API.Routes.Internal.Brig as BrigIRoutes
import qualified Wire.API.Routes.Public.Brig as BrigRoutes
import qualified Wire.API.Routes.Public.Cannon as CannonRoutes
import qualified Wire.API.Routes.Public.Cargohold as CargoholdRoutes
import qualified Wire.API.Routes.Public.Galley as GalleyRoutes
import qualified Wire.API.Routes.Public.Gundeck as GundeckRoutes
import qualified Wire.API.Routes.Public.Proxy as ProxyRoutes
-- import qualified Wire.API.Routes.Internal.Cannon as CannonIRoutes
-- import qualified Wire.API.Routes.Internal.Cargohold as CargoholdIRoutes
-- import qualified Wire.API.Routes.Internal.LegalHold as LegalHoldIRoutes
import qualified Wire.API.Routes.Public.Spar as SparRoutes

------------------------------

main :: IO ()
main = do
  writeFile "wire-fedcalls.dot" . D.renderDot . mkDotGraph $ calls
  writeFile "wire-fedcalls.csv" . toCsv $ calls

calls :: [MakesCallTo]
calls = assert (calls' == nub calls') calls'
  where
    calls' = mconcat $ parse <$> swaggers

swaggers :: [Swagger]
swaggers =
  [ -- TODO: introduce allSwaggerDocs in wire-api that collects these for all
    -- services, use that in /services/brig/src/Brig/API/Public.hs instead of
    -- doing it by hand.

    BrigRoutes.brigSwagger, -- TODO: s/brigSwagger/swaggerDoc/ like everybody else!
    CannonRoutes.swaggerDoc,
    CargoholdRoutes.swaggerDoc,
    GalleyRoutes.swaggerDoc,
    GundeckRoutes.swaggerDoc,
    ProxyRoutes.swaggerDoc,
    SparRoutes.swaggerDoc,
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

parse :: Swagger -> [MakesCallTo]
parse =
  mconcat
    . fmap parseOperationExtensions
    . mconcat
    . fmap flattenPathItems
    . HM.toList
    . _swaggerPaths

-- | extract path, method, and operation extensions
flattenPathItems :: (FilePath, PathItem) -> [((FilePath, String), HM.InsOrdHashMap Text Value)]
flattenPathItems (path, item) =
  filter ((/= mempty) . snd) $
    catMaybes
      [ ((path, "get"),) . _operationExtensions <$> _pathItemGet item,
        ((path, "put"),) . _operationExtensions <$> _pathItemPut item,
        ((path, "post"),) . _operationExtensions <$> _pathItemPost item,
        ((path, "delete"),) . _operationExtensions <$> _pathItemDelete item,
        ((path, "options"),) . _operationExtensions <$> _pathItemOptions item,
        ((path, "head"),) . _operationExtensions <$> _pathItemHead item,
        ((path, "patch"),) . _operationExtensions <$> _pathItemPatch item
      ]

parseOperationExtensions :: ((FilePath, String), HM.InsOrdHashMap Text Value) -> [MakesCallTo]
parseOperationExtensions ((path, method), hm) = uncurry (MakesCallTo path method) <$> findCallsFedInfo hm

findCallsFedInfo :: HM.InsOrdHashMap Text Value -> [(String, String)]
findCallsFedInfo hm = case A.parse parseJSON <$> HM.lookup "wire-makes-federated-call-to" hm of
  Just (A.Success (fedcalls :: [(String, String)])) -> fedcalls
  Just bad -> error $ "invalid extension `wire-makes-federated-call-to`: expected `[(comp, name), ...]`, got " <> show bad
  Nothing -> []

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
    itemTargetNode (MakesCallTo _ _ comp name) = "[" <> comp <> "]:" <> name

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
