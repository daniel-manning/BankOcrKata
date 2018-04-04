module Lib
    ( createDigit, readAccount, g, checksum, isValid, validateString, describeAccount, everyCharacterWithHammingDistance, readPossibleDigits, readAndTestAccount
    ) where

import qualified Data.Map.Strict as Map
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Control.Monad

characterMap = Map.fromList [([[0,0,0],[0,0,1],[0,0,1]], 1), ([[0,1,0],[0,1,1],[1,1,0]], 2), ([[0,1,0],[0,1,1],[0,1,1]], 3), ([[0,0,0],[1,1,1],[0,0,1]], 4), ([[0,1,0],[1,1,0],[0,1,1]], 5), ([[0,1,0],[1,1,0],[1,1,1]], 6), ([[0,1,0],[0,0,1],[0,0,1]], 7), ([[0,1,0],[1,1,1],[1,1,1]], 8), ([[0,1,0],[1,1,1],[0,1,1]], 9), ([[0,1,0],[1,0,1],[1,1,1]], 0)]

f c = if c == '|' || c == '_' then 1 else 0

createDigit::[String] -> Maybe Int
createDigit s = Map.lookup ((map.map) f s) characterMap

readAccount::[String] -> String
readAccount s = map (maybe '?' intToDigit . createDigit) (g chunks)
  where
    chunks::[[String]]
    chunks = map (chunksOf 3) s

g::[[[a]]] -> [[[a]]]
g [] = []
g [[x],[y],[z]] = [[x,y,z]]
g [x,y,z] = [head x, head y, head z] : g [tail x, tail y, tail z]

checksum::[Int] -> Int
checksum = (flip mod 11) . (foldr (\k l ->  l + uncurry (*) k) 0) . (zip [1,2..]) . reverse

isValid::[Int] -> Bool
isValid = (== 0).checksum

validateString::String -> Bool
validateString s = (notElem '?' s) && isValid (map digitToInt s)

data Result = Valid String | Missing String | Error String

analyseString::String -> Result
analyseString s | elem '?' s = Missing s
                | isValid (map digitToInt s) = Valid s
                | otherwise = Error s


extraDescription::String -> String
extraDescription s | elem '?' s = " ILL"
                   | not $ validateString s = " ERR"
                   | otherwise = ""

describeAccount::[String] -> String
describeAccount s = k ++ (extraDescription k)
                      where k = (readAccount s)

everyCharacterWithHammingDistance::Int -> [[Int]] -> [Int]
everyCharacterWithHammingDistance d character = map snd $ filter ((d ==).fst) $ map (\n -> (hammingDistance (concat character) (concat $ fst n), snd n)) $ Map.toList characterMap

hammingDistance::(Eq a) => [a] -> [a] -> Int
hammingDistance xs ys = length $ filter id $ zipWith (/=) xs ys

readPossibleDigits::[String] -> [[Int]]
readPossibleDigits s = map (\l -> (maybeToList (fst l)) ++ (everyCharacterWithHammingDistance 1 (snd l))) $ map (\k -> (createDigit k, ((map.map) f k)))(g chunks)
  where
      chunks = map (chunksOf 3) s

readOnlyMissingDigits :: [String] -> [[Int]]
readOnlyMissingDigits s = map (\l -> maybe (everyCharacterWithHammingDistance 1 (snd l)) (:[]) (fst l)) $ map (\k -> (createDigit k, ((map.map) f k)))(g chunks)
  where
      chunks = map (chunksOf 3) s

--tryAllPossibilities::[[Int]]
tryAllPossibilities s l = do l1 <- l !! 0
                             l2 <- l !! 1
                             l3 <- l !! 2
                             l4 <- l !! 3
                             l5 <- l !! 4
                             l6 <- l !! 5
                             l7 <- l !! 6
                             l8 <- l !! 7
                             l9 <- l !! 8
                             let r = [l1,l2,l3,l4,l5,l6,l7,l8,l9]
                             guard $ isValid(r)
                             guard $ ((==1) $ length $ filter (=='?') s) || ((<= 1) $ hammingDistance r (map digitToInt s))
                             return r

readAndTestAccount::[String] -> String
readAndTestAccount k =  case (analyseString(s))  of
                        Valid s -> s
                        Missing s -> formatChoices s $ (map.map) intToDigit (tryAllPossibilities s $ readOnlyMissingDigits k)
                        Error s -> formatChoices s $ (map.map) intToDigit (tryAllPossibilities s $ readPossibleDigits k)
   where s = readAccount k

formatChoices::String -> [String] -> String
formatChoices s xs = if((1==) $ length xs) then head xs else (s ++ " AMB " ++ (show $ sort xs))
