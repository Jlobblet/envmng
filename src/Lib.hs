{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Exception (IOException, catch, throwIO)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

dup :: a -> (a, a)
dup a = (a, a)

withErrorCtx :: IO a -> String -> IO a
x `withErrorCtx` str = catch x addContext
  where
    addContext :: IOException -> IO a
    addContext err = hPutStrLn stderr str >> throwIO err

envMngDir :: IO FilePath
envMngDir = do
  home <- getHomeDirectory `withErrorCtx` "Could not find home directory:"
  pure $ home </> ".envmng"