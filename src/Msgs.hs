module Msgs where

forgotPathMsg :: String
forgotPathMsg =
  "Uh-Oh! It looks like you missed the path parameter."

badPathMsg :: String
badPathMsg =
  "Error: the specified path does not exists or is invalid."

sumAndTotMsg :: String
sumAndTotMsg =
  "WARNING: you are using \"-s\" and \"-t\" at the same time."


globalAndFilePathMsg :: String
globalAndFilePathMsg =
  "WARNING: \"-g\" is ignored, because the target is a file and not " ++
  "a directory."
