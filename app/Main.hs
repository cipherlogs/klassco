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
    putStrLn ("  -t, --test \t\t small desc of what this thing do.")
    putStrLn ("  -m, --mode \t\t small desc of what this thing do.")
    putStrLn ("  -h, --help \t\t show usage and all options.")

    putStrLn ("\n\nDocumentation can be found at " ++ repoUrl)

printMessage :: String -> IO ()
printMessage msg =
  do
    putStrLn (msg)

    putStr ("\ntry running \"klassco --help\" ")
    putStrLn ("to see the usage information.")
    putStrLn ("Happy computing!")

parseArgs :: [String] -> (Maybe String, [String])
parseArgs [] = (Nothing, [])
parseArgs (x:xs) = (Just x, xs)

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
    let (userPath, options) = parseArgs args
    let defaultExt = ["html", "js", "jsx", "tsx", ".txt"]

    maybe (printMessage badArgsMsg)
          (\path ->
            do
              parsedPath <- parsePathWith defaultExt path
              maybe (printMessage badPathMsg)
                    (mapM_ (putStrLn . show) <=< getClasses)
                    (parsedPath)
          )
          (userPath)

  where badArgsMsg :: String
        badArgsMsg = "Uh-Oh! It looks like you missed the path parameter."

        badPathMsg :: String
        badPathMsg =
          "Error: the specified path does not exists or is invalid."
