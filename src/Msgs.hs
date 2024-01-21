module Msgs where

forgotPathMsg :: String
forgotPathMsg =
  "Uh-Oh! you missed the path parameter."

badPathMsg :: String
badPathMsg =
  "the specified path does not exists or is invalid."

sumAndTotMsg :: String
sumAndTotMsg =
  "you are using \"-s\" and \"-t\" at the same time."


globalAndFilePathMsg :: String
globalAndFilePathMsg =
  "\"-g\" is ignored, because the target is a file and not " ++
  "a directory."
