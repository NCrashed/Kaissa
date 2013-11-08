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
{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts #-}
module Parser.RawTransformer(
      transformRaw
    ) where

import Data.Char
import Data.Functor
import Text.Peggy hiding (space)
import Debug.Trace

-- Old and complex
{-
convertAssocs :: String -> String
convertAssocs s = strip from ++ [tab] ++ strip to ++ [tab] ++ strip weight
    where 
        tab = chr 9
        (from, stail) = break (== tab) s
        (to1, to2) = break (== tab) (tail stail)
        rawto = if to1 == " ()," 
            then init to2
            else init to1
        (to, wtail) = break (== '(') (tail rawto)
        (strWeight, _) = break (== ')') $ tail wtail
        weight = strWeight
-}

[peggy|
space :: () = [ ] { () }

top :: String 
    = from "\t(),\t" toAndWeight { $1 ++ "\t" ++ fst $2 ++ "\t" ++ show (snd $2) }
    / from "\t" toAndWeight "\t()," { $1 ++ "\t" ++ fst $2 ++ "\t" ++ show (snd $2) }

from :: String = (!"\t" anyChar)+  { $1 }

toAndWeight :: (String, Int) = toStr "(" number ")" {($1, $2)}

toStr :: String = (!"\t" anyChar)+  { $1 }

number ::: Int = [0-9]+ { read $1 }
|]



convertAssocs :: String -> String
convertAssocs input = trace input $ case parseString top "" input of
    Left err -> error $ show err
    Right ret -> ret


strip :: String -> String
strip = stripFront . stripBack
        where 
            stripFront [] = []
            stripFront (h:sx) = if h == ' ' then stripFront sx else h:sx
            stripBack [] = []
            stripBack s = if last s == ' ' then stripBack $ init s else s

transformRaw :: FilePath -> FilePath -> IO ()
transformRaw sourceFile distFile = do
    source <- lines <$> readFile sourceFile
    writeFile distFile $ unlines $ map convertAssocs source