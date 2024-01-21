import Control.Concurrent (forkIO, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Monad (when)
import System.Timeout (timeout)

generateElements :: Int -> IO ()
generateElements timeoutDuration = do
    progress <- newEmptyMVar
    done <- newEmptyMVar

    _ <- forkIO $ do
        let elements = [1..]
        mapM_ (\element -> do
            putMVar progress element
            ) elements
        putMVar done ()

    _ <- forkIO $ loop progress done 0

    _ <- timeout timeoutDuration (takeMVar done)
    return ()
  where
    loop progress done count = do
        _ <- takeMVar progress
        doneSignal <- tryTakeMVar done
        let count' = count + 1
        when (count' `mod` 1000 == 0) $ putStrLn $ "Generated " ++ show count' ++ " elements so far..."
        case doneSignal of
            Just _ -> putStrLn "Generation of elements is complete."
            Nothing -> loop progress done count'
