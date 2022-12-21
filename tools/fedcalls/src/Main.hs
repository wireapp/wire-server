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

import Control.Exception (ErrorCall (ErrorCall), throwIO)
import Data.Aeson as A
import qualified Data.ByteString.Lazy as BS (getContents)
import qualified Data.Map as M
import Imports
import Language.Dot as D

main :: IO ()
main = do
  either (throwIO . ErrorCall . show) (putStr . D.renderDot . convert) . A.eitherDecode' =<< BS.getContents

convert :: A.Value -> D.Graph
convert _ =
  -- TODO: extract actual data from swagger.
  mkDotGraph
    [ (("n1", "ep1"), "fep1"),
      (("n1", "ep1"), "fep3"),
      (("n2", "ep1"), "fep2"),
      (("n3", "ep4"), "fep2")
    ]

-- | (this function can be simplified by tossing the serial numbers for nodes, but they might
-- be useful for fine-tuning the output or rendering later.)
mkDotGraph :: [((String, String), String)] -> D.Graph
mkDotGraph inbound = Graph StrictGraph DirectedGraph Nothing (nodes <> edges)
  where
    nodes = (mkCallingNode <$> M.toList callingNodes) <> (mkCalledNode <$> M.toList calledNodes)
    edges = mkEdge <$> inbound

    callingNodes :: Map (String, String) Integer
    callingNodes =
      foldl
        (\mp (i, caller) -> M.insert caller i mp)
        mempty
        ((zip [0 ..] . nub $ fst <$> inbound) :: [(Integer, (String, String))])

    calledNodes :: Map String Integer
    calledNodes =
      foldl
        (\mp (i, called) -> M.insert called i mp)
        mempty
        ((zip [(fromIntegral $ M.size callingNodes) ..] . nub $ snd <$> inbound) :: [(Integer, String)])

    mkCallingNode :: ((String, String), Integer) -> Statement
    mkCallingNode n =
      NodeStatement (mkCallingNodeId n) []

    mkCallingNodeId :: ((String, String), Integer) -> NodeId
    mkCallingNodeId ((service, caller), i) =
      NodeId (NameId . show $ show i <> ": " <> service <> "/" <> caller) (Just (PortC CompassW))

    mkCalledNode :: (String, Integer) -> Statement
    mkCalledNode n =
      NodeStatement (mkCalledNodeId n) []

    mkCalledNodeId :: (String, Integer) -> NodeId
    mkCalledNodeId (callee, i) =
      NodeId (NameId . show $ show i <> ": " <> callee) (Just (PortC CompassE))

    mkEdge :: ((String, String), String) -> Statement
    mkEdge (caller, callee) =
      EdgeStatement
        [ ENodeId NoEdge (mkCallingNodeId (caller, callerId)),
          ENodeId DirectedEdge (mkCalledNodeId (callee, calleeId))
        ]
        []
      where
        callerId = fromMaybe (error "impossible") $ M.lookup caller callingNodes
        calleeId = fromMaybe (error "impossible") $ M.lookup callee calledNodes
