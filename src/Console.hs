module Console where

import System.Console.ANSI
import System.IO (stdout)

data LogFormat = LogFormat {color :: Color, bold :: Bool}

log' :: LogFormat -> String -> IO ()
log' format text = do
  hANSI <- hSupportsANSI stdout

  if not hANSI
     then putStr text
     else do
       setSGR [
               SetColor Foreground Vivid (color format),
               SetConsoleIntensity (if bold format then BoldIntensity else NormalIntensity)
              ]
       putStr text
       setSGR [Reset]


logBold = log' (LogFormat White True)
logError = log' (LogFormat Red True)
logSuccess = log' (LogFormat Green True)
logWarning = log' (LogFormat Yellow True)
logMain = log' (LogFormat White False)
