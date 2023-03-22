module Lib where

import Data.List
import Data.Bool
import Text.Regex.TDFA

infixl 1 ?
(?) :: Bool -> (a, a) -> a
(?) cond (x, y) = if cond then x else y

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

nth :: a -> Int -> [a] -> a
nth fallback _ [] = fallback
nth _ index xs = xs !! index

thrush :: a -> (a -> b) -> b
thrush x f = f x

nestMap :: (a -> a -> a) -> [a] -> [a] -> [a]
nestMap f xs ys = concatMap (\x -> map (\y -> f x y) ys) xs

takeEnd :: Int -> [a] -> [a]
takeEnd n xs = reverse . take n . reverse $ xs

comboProduct :: [String] -> [String] -> [String]
comboProduct xs ys = nub $ nestMap joinWithSpace xs ys

joinWithSpace :: String -> String -> String
joinWithSpace x y =
  if x > y
     then y ++ " " ++ x
     else x ++ " " ++ y

getCombos :: Int -> [String] -> [String]
getCombos minLenght xs
  | minLenght == 0 = []
  | minLenght == 1 = xs
  | otherwise = comboProduct (getCombos (minLenght - 1) xs) xs

data Sort = Asc | Desc deriving Eq

sortBy :: Ord b => Sort -> (a -> b) -> [a] -> [a]
sortBy mode f xs
  | mode == Asc = sortOn f xs
  | mode == Desc = reverse . sortOn f $ xs


splitBySpace :: [String] -> [[String]]
splitBySpace = map words

contained :: (Eq a) => [a] -> [a] -> Bool
contained [] _ = False
contained _ [] = False
contained sublist list = all (`elem` list) sublist

concatClassNames :: [String] -> String
concatClassNames = intercalate ", " . sort

countOccurences :: [[String]] -> [[String]] -> [(String, Int)]
countOccurences combos target = map countAndFormat combos

  where countAndFormat :: [String] -> (String, Int)
        countAndFormat x = (concatClassNames x, countDuplicates x target)

        countDuplicates :: [String] -> [[String]] -> Int
        countDuplicates x xs = foldl (incReducer x) 0 xs

        incReducer :: [String] -> Int -> [String] -> Int
        incReducer sublist counter list =
          bool (counter) (counter + 1) (contained sublist list)


findDuplicates :: [[String]] -> [[String]] -> [(String, Int)]
findDuplicates combos target =
  filter ((>1) . snd) $ countOccurences combos target

getUniqClasses :: [[String]] -> [String]
getUniqClasses = nub . concat

anyPredicates :: [(a -> Bool)] -> a -> Bool
anyPredicates predicates x = any (thrush x) predicates

filter' :: [(a -> Bool)] -> [a] -> [a]
filter' filters = filter (anyPredicates filters)

isUniq :: (Eq a ) => [a] -> Bool
isUniq xs = length xs == length (nub xs)

getSubMatches :: String -> String -> [String]
getSubMatches regex = getAllTextSubmatches . (=~ regex)

getMatches :: String -> String -> [String]
getMatches regex = getAllTextMatches . (=~ regex)

getClassNames :: String -> [[String]]
getClassNames =
  map (words . (nth "" 1) . getSubMatches regexPattern)
  . getMatches regexPattern

  where
    regexPattern :: String
    regexPattern = "[class|className]=\"([^\"]*)\""

genCombos :: Int -> [String] -> [[String]]
genCombos 0 _ = []
genCombos 1 xs = map (: []) xs
genCombos n xs =
  concatMap (\(x, i) -> prod' [x] (genCombos (n-1) (drop i xs)))
  . zipWith (,) xs
  $ [1..length xs]

prod' :: [String] -> [[String]] -> [[String]]
prod' xs ys = map (xs ++) ys

adjacentPairs :: Int -> [a] -> [[a]]
adjacentPairs _ [] = []
adjacentPairs size xs | size > length xs = []
adjacentPairs size xs = take size xs : adjacentPairs size (tail xs)

myCombos :: [[String]]
myCombos =
  filter' [isUniq]
  . splitBySpace
  . getCombos 2
  . getUniqClasses
  $ rawData

-- newCombos :: [[String]]
-- newCombos = genCombos 2 . getUniqClasses $ rawData

sampleData :: [String]
sampleData = ["a", "b", "c", "d"]

rawData :: [[String]]
rawData =
  [
    ["class1", "class2", "class3"],
    ["class1", "class4", "class2"],
    ["class1"],
    ["class2", "class3", "class4"]
  ]

combo1 =
  filter' [isUniq]
  . splitBySpace
  . getCombos 3
  $ sampleData

combo2 = genCombos 3 $ sampleData

-- main :: IO ()
-- main = print (genCombos 3 sampleData)
