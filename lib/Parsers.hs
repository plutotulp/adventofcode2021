module Parsers
  ( Parser
  , parseFileOrErr
  , runWithParsedInput
  , decimal
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseFileOrErr :: Parsers.Parser a -> FilePath -> IO a
parseFileOrErr p name =
  either (error . show) id . parse p name <$> TextIO.readFile name

runWithParsedInput :: FilePath -> Parsers.Parser a -> (a -> IO ()) -> IO ()
runWithParsedInput inputFile parser = (Parsers.parseFileOrErr parser inputFile >>=)

decimal :: Parser Int
decimal = L.lexeme space L.decimal
