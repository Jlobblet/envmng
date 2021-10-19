module Opts where

import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Data.Text (unpack)
import qualified Data.Text as T
import Lib (envMngDir, withErrorCtx)
import Parsers (Mode (..))
import System.Directory (getDirectoryContents, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (FilePath, combine)
import Text.Regex.PCRE ((=~))

handleOpts :: Mode -> IO ExitCode
handleOpts (List f) = do
  dir <- envMngDir
  identifiers <- filter (getPred f) <$> dirContents dir
  contents <- mapM dirContents (combine dir <$> identifiers)
  putStrLn $ intercalate "\n" $ show . Environment <$> zip identifiers (findScripts <$> contents)
  pure ExitSuccess

getPred :: Maybe T.Text -> FilePath -> Bool
getPred Nothing _ = True
getPred (Just t) s = s =~ unpack t

newtype Environment = Environment (FilePath, Scripts)

instance (Show Environment) where
  show (Environment (fp, Activate)) = "+" ++ fp
  show (Environment (fp, Deactivate)) = "-" ++ fp
  show (Environment (fp, Both)) = "+" ++ fp ++ "\n-" ++ fp
  show (Environment (fp, Neither)) = ""

data Scripts
  = Activate
  | Deactivate
  | Both
  | Neither

findScripts :: Foldable t => t FilePath -> Scripts
findScripts fps
  | activate && deactivate = Both
  | activate = Activate
  | deactivate = Deactivate
  | otherwise = Neither
  where
    activate = "activate" `elem` fps
    deactivate = "deactivate" `elem` fps

dirContents :: FilePath -> IO [FilePath]
dirContents dir = listDirectory dir `withErrorCtx` ("Could not read contents of " ++ dir)
