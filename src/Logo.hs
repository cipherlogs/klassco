module Logo (printLogo) where

logoData :: [String]
logoData =
  [
  "",
  "",
  "K K K     LL           A          SSSS    SSSS      CCCC       OOO",
  "KK        LL          A A        S       S          C        O   O",
  "K K       LL         A   A        SSSS    SSSS      C        O   O",
  "K  K      LLl       AAAAAAA          S       S      C        O   O",
  "K   k     LLLLL    A       A      SSSS    SSSS      CCCC       OOO",
  "",
  ""
  ]

printLogo :: IO ()
printLogo = mapM_ putStrLn logoData
