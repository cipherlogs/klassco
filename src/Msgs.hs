module Msgs where

forgotPathMsg :: String
forgotPathMsg =
  "Uh-Oh! It looks like you missed the path parameter."

badPathMsg :: String
badPathMsg =
  "Error: the specified path does not exists or is invalid."

displayBothMsg :: String
displayBothMsg =
  "\nWarning: you have used \"-f\" and \"-t\" at the same time.\n" ++
  "\"-t\" will now instead calculate the total number of " ++
  "duplicated combos instead"
