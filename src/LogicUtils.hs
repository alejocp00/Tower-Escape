{-# LANGUAGE ImportQualifiedPost #-}

module LogicUtils (playSound) where

import Control.Exception (catch)
import Control.Monad qualified
import System.IO.Error (isDoesNotExistError)
import System.Process (system)
import System.Info (os)

-- | for playing sounds
playSound :: FilePath -> IO ()
playSound path = do
  let cmd = case os of
        "mingw32" -> "powershell -c \"(New-Object Media.SoundPlayer '" ++ path ++ "').PlaySync();\""
        "darwin"  -> "afplay " ++ show path
        _         -> "aplay " ++ show path ++ " 2>/dev/null"
  Control.Monad.void (system cmd) `catch` handleIOError

-- | handle error
handleIOError :: IOError -> IO ()
handleIOError e
  | isDoesNotExistError e = return ()
  | otherwise = ioError e