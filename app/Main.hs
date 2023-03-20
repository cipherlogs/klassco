module Main where

import System.Environment
import System.Directory
import System.FilePath ((</>))
import Control.Monad ((<=<), (>=>))
import Data.List (isSuffixOf, isPrefixOf, sortOn, intercalate)
import Data.Maybe
import Text.Read
import Lib
import Msgs
import Logo


type ClassData = (FilePath, [[String]])
type ClassDuplicates = (FilePath, [(String, Int)])

printHelp :: IO ()
printHelp =
  do
    let repoUrl = "https://github.com/cipherlogs/Klassco"

    printLogo

    putStr ("extract all CSS classes (i.e tailwind) that ")
    putStrLn ("were used,")
    putStrLn ("analyze them and highlight all the duplicates,")
    putStr ("then abstract it into a higher abstraction ")
    putStrLn ("and make it reusable.")


    putStrLn ("\n\nUsage: klassco [options] path")

    putStrLn ("\npath:")
    putStrLn ("  the path to analyze and scan for duplicate CSS classes.")
    putStr ("  it will automatically detect whether the path is ")
    putStrLn ("a directory or a file.")

    putStrLn ("\n\noptions: [optional]")
    putStrLn ("  -m, --min INTEGER ~ (default: 2)")
    putStrLn ("\tminimum number of class combinations to check for.")

    putStrLn ("\n  -e, --extensions ~ (default: \"html js jsx\")")
    putStrLn ("\tonly look for files with these extensions.")
    putStrLn ("\tseperate extensions with space.")

    putStrLn ("\n      --asc")
    putStrLn ("\tsorts the result in ascending order.")

    putStrLn ("\n      --desc")
    putStrLn ("\tsorts the result in descending order.")

    putStrLn ("\n  -d, --display INTEGER")
    putStrLn ("\tdisplay n lines from the output, 0 to show all.")

    putStrLn ("\n  -s, --summary")
    putStrLn ("\tgenerate a summary report of each file analyzed and how")
    putStrLn ("\tduplicate combos were found.")

    putStrLn ("\n  -h, --help")
    putStrLn ("\tshow usage and all of Klassco options.")

    putStrLn ("\n  -v, --version")
    putStrLn ("\tdisplay the current version of Klassco.")

    putStrLn ("\n\nDocumentation can be found at " ++ repoUrl)

printMessage :: String -> IO ()
printMessage msg =
  do
    putStrLn (msg)

    putStr ("\ntry running \"klassco --help\" ")
    putStrLn ("to see the usage information.")
    putStrLn ("Happy computing!")

data CliFlag =
  Fhelp |
  Fversion |
  Fmin |
  Fextensions |
  Fsummary |
  Fasc |
  Fdesc |
  Fdisplay
  deriving Eq

parseFlag :: String -> Maybe CliFlag
parseFlag flag
  | flag == "-h" || flag == "--help" = Just Fhelp
  | flag == "-v" || flag == "--version" = Just Fversion
  | flag == "-m" || flag == "--min" = Just Fmin
  | flag == "-e" || flag == "--extensions" = Just Fextensions
  | flag == "-s" || flag == "--summary" = Just Fsummary
  | flag == "-d" || flag == "--display" = Just Fdisplay
  | flag == "--asc" = Just Fasc
  | flag == "--desc" = Just Fdesc
  | otherwise = Nothing

parseArgs :: [String] -> ([CliFlag], Maybe String)
parseArgs [] = ([Fhelp], Nothing)

parseArgs [x]
  | isFlag x = (mapMaybe parseFlag [x], Nothing)
  | otherwise = ([], Just x)
  where isFlag :: String -> Bool
        isFlag = isPrefixOf "-"

parseArgs xs
  | "-h" `elem` xs || "--help" `elem` xs = ([Fhelp], Nothing)
  | "-v" `elem` xs || "--version" `elem` xs = ([Fversion], Nothing)
  | otherwise = (mapMaybe parseFlag $ init xs, Just $ last xs)

getValOf :: [String] -> [String] -> Maybe String
getValOf _ args | length args < 2 = Nothing
getValOf flags (arg1:arg2:restArgs)
  | any (== arg1) flags = Just arg2
  | otherwise = getValOf flags (arg2:restArgs)

getFiles :: FilePath -> IO [FilePath]
getFiles dir =
  do
    files <- getDirectoryContents dir
    return $ map (dir </>) files

filterByExt :: [String] -> [FilePath] -> [FilePath]
filterByExt exts files = filter (hasExt exts) files

  where hasExt :: [String] -> FilePath -> Bool
        hasExt exts file = any (`isSuffixOf` file) exts


parsePathWith :: [String] -> FilePath -> IO (Maybe [FilePath])
parsePathWith exts path =
  do
    isFile <- doesFileExist path
    isDir <- doesDirectoryExist path
    let isFound = isFile || isDir

    if isFound
       then isFile ? (return (Just [path]), getContent path)
       else return Nothing

    where getContent :: FilePath -> IO (Maybe [FilePath])
          getContent path =
            do
              files <- getFiles path
              return (Just $ filterByExt exts files)

getClasses :: [FilePath] -> IO [ClassData]
getClasses = do mapM go

  where go :: FilePath -> IO ClassData
        go file =
          do
            fileContent <- readFile file
            return (file, getClassNames fileContent)

printClasses :: ClassDuplicates -> IO ()
printClasses (file, classes) =
  do
    putStr ("\n+ ")
    putStrLn (file ++ ":")
    mapM_ printClass classes

  where printClass :: (String, Int) -> IO ()
        printClass ("", count) =
          do
            putStrLn ("\tfound " ++ show count ++ " duplicates.")

        printClass (classNames, count) =
          do
            putStrLn ("\n\t" ++ classNames)
            putStrLn ("\tduplicated " ++ show count ++ " times.")

data Spec = Spec {
  minCombos :: Int,
  summary :: (ClassDuplicates -> ClassDuplicates),
  sort :: [(String, Int)] -> [(String, Int)]
}

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs args
  | Fhelp `elem` options = printHelp
  | Fversion `elem` options = putStrLn ("Klassco version: 7.1.0")
  | userPath == Nothing = printMessage forgotPathMsg
  | otherwise =
    do
      let getExtVal = getValOf ["-e", "--extensions"]
      let getMinVal = getValOf ["-m", "--min"] >=> readMaybe
      let getDisplayVal = getValOf ["-d", "--display"] >=> readMaybe
      let exts = maybe [] words $ getExtVal args :: [String]
      let minCombo = fromMaybe 2 $ getMinVal args :: Int
      let maxDisplay = fromMaybe 0 $ getDisplayVal args :: Int

      let displayN n
            | n == 0 = id
            | otherwise = (\(file, xs) -> (file, take n xs))

      let sortMethod
            | Fasc `elem` options = sortBy Asc
            | Fdesc `elem` options = sortBy Desc
            | otherwise = const id

      let specs = Spec {
          minCombos = minCombo,
          summary = Fsummary `elem` options ? (summarize, id),
          sort = sortMethod (\(_, x) -> x)
       }

      let toOutput =
            mapM_ printClasses
            . fmap (displayN maxDisplay)
            . (\x -> isSummarized x ? (sortMethod getSummaryCount x, x))
            . fmap (process specs)

      parsedPath <- parsePathWith exts (fromJust userPath)
      maybe (printMessage badPathMsg)
            (toOutput <=< getClasses)
            (parsedPath)

  where (options, userPath) = parseArgs args

        summarize :: ClassDuplicates -> ClassDuplicates
        summarize (file, xs) = (file, [("", length xs)])

        isSummarized :: [ClassDuplicates] -> Bool
        isSummarized =
          (== 1)
          . length
          . fromMaybe []
          . fmap snd
          . listToMaybe

        getSummaryCount :: ClassDuplicates -> Int
        getSummaryCount (file, xs)
          | length xs == 1 = (snd . head $ xs)
          | otherwise = (snd . head $ xs)


getDuplicates :: Int -> [[String]] -> [(String, Int)]
-- this to show when we try to get all duplicates together
-- getDuplicates min rawData = [(intercalate " " (concat combos), 2)]
getDuplicates min rawData = findDuplicates combos rawData
  where
    combos = genCombos min . getUniqClasses $ rawData
    -- combos =
    --   filter' [isUniq]
    --   . splitBySpace
    --   . getCombos min
    --   . getUniqClasses
    --   $ rawData

process :: Spec -> ClassData -> ClassDuplicates
process spec (file, rawData) =
  (\(file, classData) -> (file, sort spec classData))
  . summary spec
  $ (file, getDuplicates (minCombos spec) rawData)
