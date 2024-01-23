module Main where

import System.Environment
import System.Directory
import System.Timeout (timeout)
import System.IO
import System.FilePath ((</>))
import Control.Monad ((<=<), (>=>), forM_)
import Control.DeepSeq (NFData, deepseq, force)
import Control.Concurrent
import Control.Parallel.Strategies
import Control.Arrow (second)
import Data.List (isSuffixOf, isPrefixOf, isInfixOf, nub, sort)
import Data.Foldable (foldl')
import Data.Maybe
import Text.Read
import Lib
import Console
import Msgs
import Logo


type ClassData = (FilePath, [[String]])
type ClassDuplicates = (FilePath, [(String, Int)])

pMap :: NFData b => (a -> b) -> [a] -> [b]
pMap = parMap rpar

printHelp :: IO ()
printHelp =
  do
    let repoUrl = "https://klassco.cipherlogs.com"

    printLogo

    putStr ("Extract all CSS classes (i.e tailwind) that ")
    putStrLn ("were used,")
    putStrLn ("Analyze them and highlight all the duplicates,")
    putStr ("Then abstract it into a higher abstraction ")
    putStrLn ("and make it reusable.")


    logBold ("\n\nUsage: ")
    putStrLn ("klassco [options] path")
    logBold ("---\n\n\n")

    putStrLn ("+ path:")
    putStrLn ("  file or directory\n")

    putStrLn ("+ options: [optional]")
    logMain ("  -m, --min INTEGER ~ (default: 2)\n")
    putStrLn ("\tminimum number of class combinations to check for.")

    logMain ("\n  -g, --global\n")
    putStrLn ("\tsearch for duplicates by comparing classes across all")
    putStrLn ("\tfiles in the project. allowing you to identify where you")
    putStrLn ("\thave repeated yourself in other places.")

    logMain ("\n      --asc\n")
    putStrLn ("\tsorts the result in ascending order.")

    logMain ("\n      --desc\n")
    putStrLn ("\tsorts the result in descending order.")

    logMain ("\n  -e, --extensions ~ (default: \"html js jsx ts tsx\")\n")
    putStrLn ("\tonly look for files with these extensions.")
    putStrLn ("\tseperate extensions with space.")

    logMain ("\n  -f, --find ~ (default: all)\n")
    putStrLn ("\tfind duplicated classes with selected prefixes/infixes.\n")
    putStrLn ("\t-f \"dark: md: header-\" seperate with space.")
    putStrLn ("\t-f all -> finds all.")
    putStrLn ("\t-f none -> finds classes with no prefixes.")
    putStrLn ("\t-f \"dark:\" -> selects classes that contain dark:")
    putStrLn ("\t-f \"dark:md:\" -> selects classes that contain both.")
    putStrLn ("\t   \"dark:md:\"==\"md:dark:\" the order is ignored!")

    logMain ("\n  -d, --display INTEGER\n")
    putStrLn ("\tdisplay n found duplications from the output.")
    putStrLn ("\t\t0 to show all.")
    putStrLn ("\t\tn to show the top n lines.")
    putStrLn ("\t\t-n to show the bottom n lines.")

    logMain ("\n  -s, --summary\n")
    putStrLn ("\tgenerate a summary report of each file analyzed and how")
    putStrLn ("\tmany duplicate combos were found.")

    logMain ("\n  -t, --total\n")
    putStrLn ("\tcalculate the total.")

    logMain ("\n  -h, --help\n")
    putStrLn ("\tshow usage and all of Klassco options.")

    logMain ("\n  -v, --version\n")
    putStrLn ("\tdisplay the current version of Klassco.")


    putStr ("\n\n+ To stay up to date, I'm on Twitter ")
    logMain ("@cipherlogs\n")

    putStr ("+ Documentation can be found at ")
    logMain (repoUrl ++ "\n")

printMessage :: String -> IO ()
printMessage msg =
  do
    logError (msg)

    putStr ("\n\ntry running \"klassco --help\" ")
    putStrLn ("to see the usage information.")

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

-- add flag option to hide
printClasses :: ClassDuplicates -> IO ()
printClasses (file, classes) | (length . filter ((>0) . snd)) classes == 0 = return ()
printClasses (file, classes) =
  do
    putStr ("\n+ ")
    logBold (file ++ ":\n")
    forM_ classes printClass

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
    logMain ("\n---\n")
    putStrLn ("Found " ++ show total ++ " in total.")

    where
      sumIt :: [ClassDuplicates] -> Int
      sumIt = foldl' f 0

      f acc (_, ys) = acc + f' ys
      f' = foldl' (\acc (_, x) -> acc + x) 0


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
      let canFilterAll = (canGlobalSearch || canShowSummary) && maxDisplay /= 0

      let sortWith
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



      let runMain paths =
            do
              hSetBuffering stdout NoBuffering

              let canShowTotal = Ftotal `elem` options
              let canShowWarning1 = canShowTotal && canShowSummary
              isFilePath <- doesFileExist $ fromJust userPath
              let canShowWarning2 = canGlobalSearch && isFilePath
              let min = minCombo
              let clean = min == 1 ? (removePrefixes, id)
              let filterClasses = filter $ filterMethod prefixes
              let cleanClassData = (\(f, x) -> (f, map (clean . filterClasses) x))

              numOfThreads <- getNumCapabilities
              setNumCapabilities numOfThreads

              let threads =
                    show numOfThreads ++ " CPU thread" ++ (numOfThreads > 1 ? ("s", ""))

              logWarning ("\nINFO: ") >> putStrLn (threads ++ " can be used!")

              if canShowWarning1
                 then logWarning "WARNING: " >> (putStrLn sumAndTotMsg)
                 else return ()

              if canShowWarning2
                 then logWarning "WARNING: " >> putStrLn globalAndFilePathMsg
                 else return ()

              putStrLn ""
              putStr "+ Scanning "
              classes <- getClasses paths
              let totalClasses = sum $ map (sum . map length . snd ) classes
              classes `deepseq` putStr "."

              let allClassData = map cleanClassData classes
              allClassData `deepseq` putStr "."


              let uniqClasses = nub . concat $ map (clean . concat . snd) allClassData
              uniqClasses `deepseq` putStr ".\n"
              putStrLn (
                        "+ " ++
                        show totalClasses ++ " found, " ++
                        show (length uniqClasses) ++ " unique"
                       )

              let combosCount = getCombosCount min uniqClasses
              putStrLn $ "+ " ++ describeComboCount combosCount ++ "\n"

              putStr ("+ Extracting\n")
              progressId <- forkIO $ showProgress 2

              let extractDuplicates = keepGt (canGlobalSearch ? (0, 1)) . go
                    where
                      go = min == 1 ? (concatMap (count . (:[])), count)
                      count = countDups min


              let cutFrom = takeN maxDisplay

              let result =
                      (canFilterAll ? (cutFrom, fmap $ second cutFrom))
                    . (\x -> isSummarized x ? (sortWith getSummaryCount x, x))
                    . (canGlobalSearch ? (countGlobalDups, id))
                    . filter (hasClasses)
                    . pMap (second $ sortWith snd)
                    . pMap (canShowSummary ? (summarize, id))
                    . pMap (second extractDuplicates)
                    $ allClassData

              --
              -- Output
              forM_ (result) printClasses
              length result == 0 ? (putStrLn "\n+ Nothing was found", return ())
              result `deepseq` killThread progressId
              canShowTotal ? (printTotal result, return ())



      parsedPath <- parsePathWith exts (fromJust userPath)
      maybe (printMessage badPathMsg)
            (runMain)
            (parsedPath)

  where (options, userPath) = parseArgs args

        showProgress :: Int -> IO ()
        showProgress interval =
          do
            threadDelay (interval * 1000000)
            putStr "."
            hFlush stdout
            showProgress interval


        hasClasses :: ClassDuplicates -> Bool
        hasClasses = (notNull `andd` ((>0) . snd . head)) . snd

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
        getSummaryCount = snd . head . snd
