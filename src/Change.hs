{-# LANGUAGE OverloadedStrings #-}

module Change
  ( runChanges,
  )
where

import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure))
import Parsers (Change (..))
import qualified Data.Text as T
import Lib (envMngDir, withErrorCtx)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Control.Monad (msum)
import System.IO (hPutStrLn)
import GHC.IO.Handle.FD (stderr)


eitherCode :: ExitCode -> Either Int ()
eitherCode ExitSuccess = Right ()
eitherCode (ExitFailure x) = Left x

runChanges :: [Change] -> IO ExitCode
runChanges chs = fmap (either ExitFailure (const ExitSuccess)) <$> msum $ map (fmap eitherCode . handleChange) chs

handleChange :: Change -> IO ExitCode
handleChange change = do
  dir <- envMngDir
  let filepath = dir </> relativePath change
  exists <- doesFileExist filepath
  if exists
    then ExitSuccess <$ putStrLn (". " ++ filepath ++ ";") `withErrorCtx` ("Could run run file:" ++ filepath)
    else ExitFailure 1 <$ hPutStrLn stderr ("Could not find file " ++ filepath)
  where
    relativePath :: Change -> FilePath
    relativePath (Add a) = T.unpack a </> "activate"
    relativePath (Remove r) = T.unpack r </> "deactivate"
