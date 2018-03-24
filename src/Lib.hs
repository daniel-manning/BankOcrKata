module Lib
    ( createDigit, readAccount, g
    ) where

import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.Maybe
import Data.Char

characterMap = Map.fromList [([[0,0,0],[0,0,1],[0,0,1]], 1), ([[0,1,0],[0,1,1],[1,1,0]], 2), ([[0,1,0],[0,1,1],[0,1,1]], 3), ([[0,0,0],[1,1,1],[0,0,1]], 4), ([[0,1,0],[1,1,0],[0,1,1]], 5), ([[0,1,0],[1,1,0],[1,1,1]], 6), ([[0,1,0],[0,0,1],[0,0,1]], 7), ([[0,1,0],[1,1,1],[1,1,1]], 8), ([[0,1,0],[1,1,1],[0,1,1]], 9), ([[0,1,0],[1,0,1],[1,1,1]], 0)]

f c = if(c == '|' || c == '_') then 1 else 0

createDigit::[String] -> Maybe Int
createDigit s = Map.lookup ((map.map) f s) characterMap

readAccount::[String] -> String
readAccount s = map intToDigit $ catMaybes $ map createDigit (g chunks)
  where
    chunks::[[String]]
    chunks = map (chunksOf 3) s

g::[[[a]]] -> [[[a]]]
g [] = []
g ([x]:[y]:[z]:[]) = [[x,y,z]]
g (x:y:z:[]) = [head x, head y, head z] : g (tail x : tail y : tail z : [])