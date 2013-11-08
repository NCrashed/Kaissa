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
module Parser.GraphLoader(
      loadGraphFromSCV
    ) where

import Data.WeightedGraph
import Data.Functor
import Data.Char


loadGraphFromSCV :: FilePath -> IO (Graph String Int)
loadGraphFromSCV fileName = foldl graphAddEdge emptyGraph . map parseEdge <$> lines <$> readFile fileName
    where 
        parseEdge :: String -> (String, String, Int)
        parseEdge s = (from, to, weight)
            where
                tab = chr 9
                (from, s') = break (== tab) s 
                (to, s'') = break (== tab) $ tail s'
                weight = read $ tail s''