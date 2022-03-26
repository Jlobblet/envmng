module Main where

import Change (runChanges)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Options.Applicative (execParser)
import Opts (handleOpts)
import Parsers (opts, parseChanges)
import System.Environment (getArgs)
import System.Exit (exitWith)

main :: IO ExitCode
main = getArgs >>= either (const parseOpts) runChanges . parseChanges
  where
    parseOpts = execParser opts >>= handleOpts
