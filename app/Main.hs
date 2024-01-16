module Main where

import System.Environment
import System.Directory
import System.FilePath ((</>))
import Control.Monad ((<=<), (>=>))
import Data.List (isSuffixOf, isPrefixOf, isInfixOf, nub)
import Data.Foldable (foldl')
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
    let repoUrl = "https://cipherlogs.github.io/klassco"

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

    putStrLn ("  -g, --global")
    putStrLn ("\tsearch for duplicates by comparing classes across all")
    putStrLn ("\tfiles in the project. allowing you to identify where you")
    putStrLn ("\thave repeated yourself in other places.")

    putStrLn ("\n      --asc")
    putStrLn ("\tsorts the result in ascending order.")

    putStrLn ("\n      --desc")
    putStrLn ("\tsorts the result in descending order.")

    putStrLn ("\n  -e, --extensions ~ (default: \"html js jsx ts tsx\")")
    putStrLn ("\tonly look for files with these extensions.")
    putStrLn ("\tseperate extensions with space.")

    putStrLn ("\n  -f, --find ~ (default: all)")
    putStrLn ("\tfind duplicated classes with selected prefixes/infixes.\n")
    putStrLn ("\t-f \"dark: md: header-\" seperate with space.")
    putStrLn ("\t-f all -> finds all.")
    putStrLn ("\t-f none -> finds classes with no prefixes.")
    putStrLn ("\t-f \"dark:\" -> selects classes that contain dark:")
    putStrLn ("\t-f \"dark:md:\" -> selects classes that contain both.")
    putStrLn ("\t   \"dark:md:\"==\"md:dark:\" the order is ignored!")

    putStrLn ("\n  -d, --display INTEGER")
    putStrLn ("\tdisplay n lines from the output.")
    putStrLn ("\t\t0 to show all.")
    putStrLn ("\t\t+n to show the top n lines.")
    putStrLn ("\t\t-n to show the bottom n lines.")

    putStrLn ("\n  -s, --summary")
    putStrLn ("\tgenerate a summary report of each file analyzed and how")
    putStrLn ("\tmany duplicate combos were found.")

    putStrLn ("\n  -t, --total")
    putStrLn ("\tcalculate the total.")

    putStrLn ("\n  -h, --help")
    putStrLn ("\tshow usage and all of Klassco options.")

    putStrLn ("\n  -v, --version")
    putStrLn ("\tdisplay the current version of Klassco.")

    putStrLn ("\n\n+ To stay up to date, I'm on Twitter @cipherlogs")
    putStrLn ("+ Documentation can be found at " ++ repoUrl)

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
  Fdisplay |
  Ffind |
  Ftotal |
  Fglobal
  deriving Eq

parseFlag :: String -> Maybe CliFlag
parseFlag flag
  | flag == "-h" || flag == "--help" = Just Fhelp
  | flag == "-v" || flag == "--version" = Just Fversion
  | flag == "-m" || flag == "--min" = Just Fmin
  | flag == "-e" || flag == "--extensions" = Just Fextensions
  | flag == "-s" || flag == "--summary" = Just Fsummary
  | flag == "-d" || flag == "--display" = Just Fdisplay
  | flag == "-f" || flag == "--find" = Just Ffind
  | flag == "-t" || flag == "--total" = Just Ftotal
  | flag == "-g" || flag == "--global" = Just Fglobal
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

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles path = do
  isDir <- doesDirectoryExist path
  if isDir
     then do
       contents <- listDirectory path
       let ignoreList = ["node_modules", ".git"]
       let safeContents = filter (not . (flip elem) ignoreList) contents
       paths <- mapM (\name -> getAllFiles (path </> name)) safeContents
       return (concat paths)

  else do
    return [path]

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

    where 
      getContent :: FilePath -> IO (Maybe [FilePath])
      getContent path =
        do
          files <- getAllFiles path
          return (Just $ filterByExt exts files)

getClasses :: [FilePath] -> IO [ClassData]
getClasses = do mapM go

  where go :: FilePath -> IO ClassData
        go file =
          do
            fileContent <- readFile file
            return (file, getClassNames fileContent)

printClasses :: ClassDuplicates -> IO ()
printClasses (file, classes) | (length . filter ((>0) . snd)) classes == 0 = return ()
printClasses (file, classes) =
  do
    putStr ("\n+ ")
    putStrLn (file ++ ":")
    mapM_ printClass classes

  where printClass :: (String, Int) -> IO ()
        printClass ("", count) =
          do
            putStrLn ("\tfound " ++ show count ++ " duplicate" ++ (count > 1 ? ("s.", ".")))

        printClass (classNames, count) =
          do
            putStrLn ("\t" ++ classNames)
            putStrLn ("\tduplicated " ++ show count ++ " times.")
            putStrLn ""

printTotal :: [ClassDuplicates] -> IO ()
printTotal xs =
  do
    let total = sumIt xs
    putStrLn ("\n---")
    putStrLn ("Found " ++ show total ++ " in total.")

    where
      sumIt :: [ClassDuplicates] -> Int
      sumIt = foldl' f 0

      f acc (_, ys) = acc + f' ys
      f' = foldl' (\acc (_, x) -> acc + x) 0

data Spec = Spec {
  minCombos :: Int,
  summary :: (ClassDuplicates -> ClassDuplicates),
  sort :: [(String, Int)] -> [(String, Int)],
  specFilter :: String -> Bool,
  getDuplicates :: [[String]] -> [[String]] -> [(String, Int)]
}

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO ()
handleArgs args
  | Fhelp `elem` options = printHelp
  | Fversion `elem` options = putStrLn ("Klassco 1.0")
  | userPath == Nothing = printMessage forgotPathMsg
  | otherwise =
    do
      let getExtVal = getValOf ["-e", "--extensions"]
      let getPrefixesVal = getValOf ["-f", "--find"]
      let getMinVal = getValOf ["-m", "--min"] >=> readMaybe
      let getDisplayVal = getValOf ["-d", "--display"] >=> readMaybe
      let defaultExts = ["html", "js", "jsx", "ts", "tsx"]
      let exts = maybe defaultExts words $ getExtVal args :: [String]
      let prefixes = maybe [] words $ getPrefixesVal args :: [String]
      let minCombo = fromMaybe 2 $ getMinVal args :: Int
      let maxDisplay = fromMaybe 0 $ getDisplayVal args :: Int
      let canGlobalSearch = Fglobal `elem` options && minCombo > 1
      let canShowSummary = Fsummary `elem` options && not canGlobalSearch

      let displayN n
            | n == 0 = id
            | n > 0 = (\(file, xs) -> (file, take n xs))
            | otherwise = (\(file, xs) -> (file, drop (length xs + n) xs))

      let sortMethod
            | Fasc `elem` options = sortBy Asc
            | Fdesc `elem` options = sortBy Desc
            | otherwise = const id

      let filterMethod opts
            | opts == [] = const (True)
            | opts == ["all"] = const (True)
            | opts == ["none"] = not . isInfixOf ":"
            | length opts >= 1 =
                \x -> any (all (`isInfixOf` x) . splitWith ':') opts

            | otherwise = const (True)

      let specs = Spec {
          minCombos = minCombo,
          summary = canShowSummary ? (summarize, id),
          sort = sortMethod (\(_, x) -> x),
          specFilter = filterMethod prefixes,
          getDuplicates =
            canGlobalSearch ? (findDuplicates 1, findDuplicates 2)
       }

      let calcOutput =
            fmap (displayN maxDisplay)
            . (\x -> isSummarized x ? (sortMethod getSummaryCount x, x))
            . (\x -> canGlobalSearch ? (processAll x, x))
            . fmap (process specs)

      let toOutput xs =
            do
              let canShowTotal = Ftotal `elem` options
              let shouldDisplayBoth = canShowTotal && canShowSummary

              shouldDisplayBoth ? (putStrLn displayBothMsg, return ())
              mapM_ printClasses xs
              canShowTotal ? (printTotal xs, return ())

      parsedPath <- parsePathWith exts (fromJust userPath)
      maybe (printMessage badPathMsg)
            ((toOutput . calcOutput) <=< getClasses)
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
        getSummaryCount (_, xs)
          | length xs == 1 = (snd . head $ xs)
          | otherwise = (snd . head $ xs)


        processAll :: [ClassDuplicates] -> [(String, [(String, Int)])]
        processAll xs = mapMaybe (fromAll xs) foundCombos

          where
            foundCombos :: [String]
            foundCombos = nub . concatMap (map fst . snd) $ xs

            getComboCount :: String -> [(String, Int)] -> Int
            getComboCount x =
              maybe 0 snd
              . listToMaybe
              . filter ((== x) . fst)

            fromAll :: [ClassDuplicates] -> String -> Maybe ClassDuplicates
            fromAll xs combo =
              (\x -> length x > 1 ? (Just (combo, x), Nothing))
              . filter ((>1) . snd)
              . map (\(file, ys) -> (file, getComboCount combo ys))
              $ xs


process :: Spec -> ClassData -> ClassDuplicates
process spec (file, rawData) =
  let numCombo = minCombos spec
      filteredData = map (filter (specFilter spec)) rawData
      uniqClasses = getUniqClasses filteredData
      combos = genCombos numCombo uniqClasses
      foundCombos = (getDuplicates spec) combos filteredData
  in
  (\(file, classData) -> (file, sort spec classData))
  . summary spec
  $ (file, foundCombos)
