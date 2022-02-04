module ParserExtensions(
exactText
) where

import Data.Text (Text)
import Telegram.Bot.Simple.UpdateParser (UpdateParser, text)

exactText :: Text -> UpdateParser Text
exactText t = do
  parserText <- text
  if t == parserText
    then pure t
    else fail "not equal"   