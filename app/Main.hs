module Main where

import System.Environment
import System.Directory
import System.FilePath ((</>))
import Control.Monad ((<=<))
import Data.List (isSuffixOf)
import Data.Maybe
import Lib
import Logo


printHelp :: Bool -> IO ()
printHelp canShowLogo =
  do
    let repoUrl = "https://github.com/cipherlogs/Klassco"

    canShowLogo ? (logo, return ())
    putStrLn ("Usage: klassco [path] [options]")

    putStrLn ("\n\nPath:")
    putStrLn ("  -t, --test \t\t small desc of what this thing do.")
    putStrLn ("  -m, --mode \t\t small desc of what this thing do.")

    putStrLn ("\nOptions:")
    putStrLn ("  -m, --min INTEGER \t\t Minimum number of class" ++
              "combinations to check for (Default: 2)")

    putStrLn ("  -m, --mode \t\t small desc of what this thing do.")
    putStrLn ("  -h, --help \t\t show usage and all options.")
    putStrLn ("  -s, --summary \t\t show usage and all options.")
    putStrLn ("  -v, --version \t\t display the current version.")

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
  Fsummary |
  Fversion |
  Fmin
  deriving Eq

parseFlag :: String -> Maybe CliFlag
parseFlag flag
  | flag == "-h" || flag == "--help" = Just Fhelp
  | otherwise = Nothing

-- maybe rewrite this so that it can look nicer.
-- how about using guards only
parseArgs :: [String] -> ([CliFlag], Maybe String)
parseArgs [] = ([Fhelp], Nothing)
parseArgs [x]
  | x == "-h" || x == "--help" = ([Fhelp], Nothing)
  | otherwise = ([], Just x)

parseArgs xs
  | "-h" `elem` xs || "--help" `elem` xs = ([Fhelp], Nothing)
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
          | Fhelp `elem` options = printHelp False
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
