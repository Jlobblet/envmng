module Main where

import Change (runChanges)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Options.Applicative (execParser)
import Parsers (opts, parseChanges)
import System.Environment (getArgs)
import System.Exit (exitWith)
import Opts (handleOpts)

main :: IO ExitCode
main = do
  changes <- parseChanges <$> getArgs
  code <- case changes of
    Left pebs -> do
      args <- execParser opts
      handleOpts args
    Right chs -> runChanges chs

  exitWith code
