module Main where

import Change (runChanges)
import GHC.IO.Exception (ExitCode)
import Options.Applicative (execParser)
import Opts (handleOpts)
import Parsers (opts, parseChanges)
import System.Environment (getArgs)

main :: IO ExitCode
main = getArgs >>= either (const parseOpts) runChanges . parseChanges
  where
    parseOpts = execParser opts >>= handleOpts
