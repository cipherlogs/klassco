module Main where

import System.Environment
import System.Directory
import System.FilePath ((</>))
import Control.Monad ((<=<))
import Data.List (isSuffixOf, isPrefixOf)
import Data.Maybe
import Lib
import Logo


printHelp :: IO ()
printHelp =
  do
    let repoUrl = "https://github.com/cipherlogs/Klassco"

    printLogo
    putStrLn ("Usage: klassco [options] path")

    putStrLn ("\n\npath:")
    putStrLn ("  the path to analyze and scan for duplicate CSS classes.")
    putStr ("  the utility will automatically detect whether the path is ")
    putStrLn ("a directory or a file.")

    putStrLn ("\n\noptions: [optional]")
    putStrLn ("  -m, --min INTEGER ~ (default: 2)")
    putStrLn ("\tminimum number of class combinations to check for.")

    putStrLn ("\n  -e, --extensions ~ (default: \"html js jsx\")")
    putStrLn ("\tonly look for files with these extensions.")
    putStrLn ("\tseperate extensions with space.")

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
  Fextensions
  deriving Eq

parseFlag :: String -> Maybe CliFlag
parseFlag flag
  | flag == "-h" || flag == "--help" = Just Fhelp
  | flag == "-v" || flag == "--version" = Just Fversion
  | flag == "-m" || flag == "--min" = Just Fmin
  | flag == "-e" || flag == "--extensions" = Just Fextensions
  | otherwise = Nothing

-- maybe rewrite this so that it can look nicer.
-- how about using guards only
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

getClasses :: [FilePath] -> IO [(FilePath, [[String]])]
getClasses = do mapM go

  where go :: FilePath -> IO (FilePath, [[String]])
        go file =
          do
            fileContent <- readFile file
            return (file, getClassNames fileContent)


main :: IO ()
main =
  do
    args <- getArgs
    handleArgs $ parseArgs args

  where handleArgs :: ([CliFlag], Maybe String) -> IO ()
        handleArgs (options, userPath)
          | Fhelp `elem` options = printHelp
          | Fversion `elem` options = putStrLn ("Klassco version: 7.1.0")
          | userPath == Nothing = printMessage forgotPathMsg
          | otherwise = do
              let defaultExt = ["html"]
              parsedPath <- parsePathWith defaultExt (fromJust userPath)

              maybe (printMessage badPathMsg)
                    (mapM_ (putStrLn . show) <=< getClasses)
                    (parsedPath)

        forgotPathMsg :: String
        forgotPathMsg =
          "Uh-Oh! It looks like you missed the path parameter."

        badPathMsg :: String
        badPathMsg =
          "Error: the specified path does not exists or is invalid."
