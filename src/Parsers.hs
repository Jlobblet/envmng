{-# LANGUAGE OverloadedStrings #-}

module Parsers
  ( opts,
    Mode (..),
    Change (..),
    parseChanges,
  )
where

import Data.Char (isLetter, isNumber, isSpace)
import Data.Either (isLeft, lefts, rights)
import qualified Data.Text as T
import Data.Void (Void)
import Options.Applicative (Parser, ParserInfo, argument, auto, command, fullDesc, header, help, helper, hsubparser, info, optional, progDesc, (<**>))
import Options.Applicative.Builder (eitherReader)
import Text.Megaparsec (MonadParsec (takeWhile1P), ParseErrorBundle, Parsec, parse, (<|>))
import Text.Regex.PCRE (Regex)

data Change
  = Add T.Text
  | Remove T.Text

instance (Show Change) where
  show (Add t) = "+" ++ show t
  show (Remove t) = "-" ++ show t

newtype Mode
  = List (Maybe T.Text)

opts :: ParserInfo Mode
opts = info (helper <*> modeParser) (fullDesc <> progDesc desc <> header "envmng")
  where
    desc =
      "envmng is used for activating and deactivating sections in a shell configuration \n\
      \file to reduce start up times. \n\
      \ \n\
      \Example usage: \n\
      \envmng +rust \n\
      \envmng +rust +go \n\
      \envmng -d \n\
      \envmng list 'sharp'"

modeParser :: Parser Mode
modeParser = hsubparser (command "list" (info listOptions (progDesc "Search available environments")))

listOptions :: Parser Mode
listOptions = List <$> optional (argument auto (help "Search term"))

parseChanges :: [String] -> Either [ParseErrorBundle String Void] [Change]
parseChanges = collectEithers . fmap (parse parseChange "")
  where
    collectEithers es = if any isLeft es then Left $ lefts es else Right $ rights es

parseChange :: Parsec Void String Change
parseChange = parseAdd <|> parseRemove

parseAdd :: Parsec Void String Change
parseAdd = Add <$> ("+" *> parseIdentifier)

parseRemove :: Parsec Void String Change
parseRemove = Remove <$> ("-" *> parseIdentifier)

parseIdentifier :: Parsec Void String T.Text
parseIdentifier = T.pack <$> takeWhile1P (Just "identifier") (or . sequence [isLetter, isNumber, (== '#')])
