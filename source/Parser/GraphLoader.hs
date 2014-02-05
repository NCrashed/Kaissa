{-# LANGUAGE DoAndIfThenElse #-}
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
import Text.Parsec
import Text.Parsec.Extra 
import Data.Text
import Data.Functor ((<$>))
import Debug.Trace (trace)

type CSVParser a = Parsec String Char a

csv :: CSVParser [(String, String, Int)]
csv = do
  result <- many line
  eof
  return result 

line :: CSVParser (String, String, Int)
line = do
  result <- cells
  eol
  return result 

firstCell :: CSVParser String
firstCell = manyTill anyChar tab

secondCell :: CSVParser String
secondCell = manyTill anyChar (char ',')
  
parseInnerCell :: CSVParser (String, Int)
parseInnerCell = do
  body <- manyTill anyChar $ try $ do
    char '('
    notFollowedBy (noneOf "0123456789") 
  
  vals <- many Text.Parsec.digit
  let val = read vals
  _ <- char ')'
  _ <- optional (char '*')
  return (body, val)
  
cells :: CSVParser (String, String, Int)
cells = do
  first  <- firstCell
  second <- secondCell
  _ <- tab
  third  <- secondCell
  if second == "()" then
    case runParser parseInnerCell ' ' "" third of
      Right (second', val) -> return (first, second', val)
      Left err -> fail $ show err
  else if third == "()" then
    case runParser parseInnerCell ' ' "" second of
      Right (second', val) -> return (first, second', val)
      Left err -> fail $ show err 
  else fail "Unexpected format!" 
  
loadGraphFromSCV :: FilePath -> IO (Graph String Int)
loadGraphFromSCV fileName = do
  raws <- getRaws
  return $ Prelude.foldl graphAddEdge emptyGraph raws
  where
    getRaws = do
      res <- runParser csv ' ' "" <$> readFile fileName 
      case res of
        Right vals -> return vals
        Left err -> fail $ show err