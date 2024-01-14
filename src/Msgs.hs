module Msgs where

forgotPathMsg :: String
forgotPathMsg =
  "Uh-Oh! It looks like you missed the path parameter."

badPathMsg :: String
badPathMsg =
  "Error: the specified path does not exists or is invalid."

displayBothMsg :: String
displayBothMsg =
  "\nWarning: you are using \"-s\" and \"-t\" at the same time.\n"
