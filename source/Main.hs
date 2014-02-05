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
module Main(main) where

import Data.WeightedGraph
import Parser.GraphLoader
import System.Environment
import UU.PPrint

graph1 :: Graph String Int
graph1 = graphFromEdges [
      ("v1", "v2", 4)
    , ("v1", "v5", 1)
    , ("v2", "v5", 2)
    , ("v3", "v2", 2)
    , ("v3", "v4", 1)
    , ("v5", "v1", 3)
    , ("v5", "v3", 1)
    ]

graph2 :: Graph String Int
graph2 = graphFromEdges [
      ("v1", "v2", 2)
    , ("v5", "v1", 1)
    , ("v5", "v2", 3)
    , ("v5", "v6", 1)
    ]

-- main = putDoc $ pretty (graphMerge graph1 graph2)

--main = do
--    args <- getArgs
--    transformRaw (args !! 0) (args !! 1)

main = do
    args <- getArgs
    graph <- loadGraphFromSCV $ args !! 0
    putDoc $ pretty graph
