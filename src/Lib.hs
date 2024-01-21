module Lib where

import Data.List
import Data.Bool
import Data.Foldable (foldl', foldr')
import Text.Regex.TDFA
import Control.Monad (guard, when, forM_)
import Control.Applicative (liftA2)
import Math.Combinat (choose)
import Text.Printf
import System.CPUTime (getCPUTime)
import System.Timeout (timeout)
import System.Console.ANSI

infixl 1 ?
(?) :: Bool -> (a, a) -> a
(?) cond (x, y) = if cond then x else y

maybeIf :: (a -> Bool) -> a -> Maybe a
maybeIf p x = x <$ guard (p x)

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

nth :: a -> Int -> [a] -> a
nth fallback _ [] = fallback
nth _ index xs = xs !! index

andd :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andd = liftA2 (&&)

orr :: (a -> Bool) -> (a -> Bool) -> a -> Bool
orr = liftA2 (||)

notNull :: Foldable t => t a -> Bool
notNull = not . null

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

concatClassNames :: [String] -> String
concatClassNames = intercalate ", " . sort

countOccurences :: [[String]] -> [[String]] -> [(String, Int)]
countOccurences combos target = map countAndFormat combos

  where countAndFormat :: [String] -> (String, Int)
        countAndFormat x = (concatClassNames x, countDuplicates x target)

        countDuplicates :: [String] -> [[String]] -> Int
        countDuplicates x xs = foldl' (incReducer x) 0 xs

        incReducer :: [String] -> Int -> [String] -> Int
        incReducer sublist counter list =
          bool (counter) (counter + 1) (contained sublist list)

contained :: (Ord a) => [a] -> [a] -> Bool
contained [] _ = False
contained _ [] = False
contained sublist list = all (`elem` list) sublist

countOccurencesEach :: [[String]] -> [[String]] -> [(String, Int)]
countOccurencesEach combos target = map countAndFormat combos

  where countAndFormat :: [String] -> (String, Int)
        countAndFormat x = (concatClassNames x, countDuplicates x target)

        countDuplicates :: [String] -> [[String]] -> Int
        countDuplicates x xs = foldl' (incReducer x) 0 xs

        incReducer :: [String] -> Int -> [String] -> Int
        incReducer sublist counter list =
          bool (counter) (counter + i) (contained sublist list)
            where i = length . filter ((==)(intercalate " " sublist)) $ list


keepGt :: Int -> [(String, Int)] -> [(String, Int)]
keepGt min = filter ((> min) . snd)

getUniqClasses :: [[String]] -> [String]
getUniqClasses = nub . concat

anyPredicates :: [(a -> Bool)] -> a -> Bool
anyPredicates predicates x = any ($ x) predicates

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

genCombos :: Int -> [a] -> [[a]]
genCombos = choose

getCombosCount :: Int -> [a] -> Integer
getCombosCount k' xs = product [(n - k + 1).. n] `div` product [1..k]
  where
    n = toInteger . length $ xs
    k = toInteger k'

secondsToTime :: Int -> String
secondsToTime seconds =
  let (hours, remainder1) = seconds `divMod` 3600
      (minutes, secs)     = remainder1 `divMod` 60
   in printf "%02dh:%02dmin:%02dsec" hours minutes secs

adjacentPairs :: Int -> [a] -> [[a]]
adjacentPairs _ [] = []
adjacentPairs size xs | size > length xs = []
adjacentPairs size xs = take size xs : adjacentPairs size (tail xs)

splitWith :: Eq a => a -> [a] -> [[a]]
splitWith seperator = foldr' go [[]]

  where
    go char acc@(x:xs)
      | char == seperator = [seperator] : acc
      | otherwise = (char : x) : xs


removePrefixes :: [String] -> [String]
removePrefixes = map clean
  where clean = reverse . takeWhile ((/=) ':') . reverse


takeN :: Int -> [a] -> [a]
takeN n xs
  | n == 0 = xs
  | n > 0 = take n xs
  | otherwise = drop (length xs + n) xs


describeComboCount :: Integer -> String
describeComboCount n
  | n >= 1000000000000000 =
    "Over " ++ show (n `div` 1000000000000000) ++ " quadrillion combos will be generated"

  | n >= 1000000000000 =
    "Over " ++ show (n `div` 1000000000000) ++ " trillion combos will be generated"

  | n >= 1000000000 =
    "Over " ++ show (n `div` 1000000000) ++ " billion combos will be generated"

  | n >= 1000000 =
    "Over " ++ show (n `div` 1000000) ++ " million combos will be generated"

  | n >= 1000 =
    "Over " ++ show (n `div` 1000) ++ " thousand combos will be generated"

  | otherwise = show n ++ " combos will be generated"



-- main :: IO ()
-- main = print . show $ countOccurences combos target
