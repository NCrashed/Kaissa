-- Copyright 2013 Gushcha Anton 
-- This file is part of PowerCom.
--
--    PowerCom is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    PowerCom is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with PowerCom.  If not, see <http://www.gnu.org/licenses/>.
module Data.WeightedGraph (
      Graph
    , emptyGraph
    , graphEdges
    , graphFromEdges
    , graphAddEdge 
    , graphMerge
    ) where

import qualified Data.Map as Map
import UU.PPrint

data Graph vertexMark edgeMark = Graph (Map.Map vertexMark (Map.Map vertexMark edgeMark))

instance (Pretty vertexMark, Pretty edgeMark) => Pretty (Graph vertexMark edgeMark) where
    pretty graph = foldl showVertex (string "Graph:\n") $ graphEdges graph
        where
            showVertex ss (from, to, weight) = ss <+> (pretty from) <+> string " -> " <+> (pretty to) <+> string " : " <+> (pretty weight) <+> string "\n"

instance (Eq vertexMark, Eq edgeMark) => Eq (Graph vertexMark edgeMark) where
    (==) (Graph mapA) (Graph mapB) = mapA == mapB  

emptyGraph :: Graph vertexMark edgeMark
emptyGraph = Graph Map.empty

graphEdges :: Graph vertexMark edgeMark -> [(vertexMark, vertexMark, edgeMark)]
graphEdges (Graph vertMap) = Map.foldrWithKey iterateVertecies [] vertMap
    where
        iterateVertecies :: vertexMark -> Map.Map vertexMark edgeMark -> [(vertexMark, vertexMark, edgeMark)] -> [(vertexMark, vertexMark, edgeMark)]
        iterateVertecies from vertexMap acc = (map (\(to, weight) -> (from, to, weight)) $ Map.assocs vertexMap) ++ acc

graphFromEdges :: (Ord vertexMark, Num edgeMark) => [(vertexMark, vertexMark, edgeMark)] -> Graph vertexMark edgeMark
graphFromEdges edges = foldl graphAddEdge emptyGraph edges
        
graphAddEdge :: (Ord vertexMark, Num edgeMark) => Graph vertexMark edgeMark -> (vertexMark, vertexMark, edgeMark) -> Graph vertexMark edgeMark
graphAddEdge (Graph vertMap) (from, to, weight) = case Map.member from vertMap of 
    False -> Graph $ Map.insert from (Map.singleton to weight) vertMap
    True  -> Graph $ Map.insert from newFromMap vertMap
    where
        newFromMap = case Map.member to fromMap of
            False -> Map.insert to weight fromMap
            True  -> Map.adjust (+ weight) to fromMap
        fromMap   = vertMap Map.! from

graphMerge :: (Ord vertexMark, Num edgeMark) => Graph vertexMark edgeMark -> Graph vertexMark edgeMark -> Graph vertexMark edgeMark
graphMerge graphA graphB = foldl graphAddEdge graphA $ graphEdges graphB