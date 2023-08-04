{-# LANGUAGE ImportQualifiedPost #-}

module LogicUtils (playSound) where

import Control.Exception (catch)
import Control.Monad qualified
import System.IO.Error (isDoesNotExistError)
import System.Process (system)

-- | for playing sounds
playSound :: FilePath -> IO ()
playSound path = do
  let command = "aplay " ++ path ++ " 2>/dev/null"
  Control.Monad.void (system command) `catch` handleIOError

-- | handle error
handleIOError :: IOError -> IO ()
handleIOError e
  | isDoesNotExistError e = return ()
  | otherwise = ioError e