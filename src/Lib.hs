{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Exception (IOException, catch, throwIO)
import Data.Functor ((<&>))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

withErrorCtx :: IO a -> String -> IO a
x `withErrorCtx` str = catch x addContext
  where
    addContext :: IOException -> IO a
    addContext err = hPutStrLn stderr str >> throwIO err

envMngDir :: IO FilePath
envMngDir =
  getHomeDirectory `withErrorCtx` "Could not find home directory:" <&> (</> ".envmng")
