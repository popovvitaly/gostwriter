{-# LANGUAGE OverloadedStrings #-}
module Main where

import ParserExtensions                 (exactText)
import ReplyExtensions                  (replyDocumentWithCaption)
import Const.Messages
import Const.Texts

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser (parseUpdate, command, text)
import Control.Applicative              ((<|>))

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction        -- ^ Perform no action.
  | Help            -- ^ Show help message and go to empty state 
  | Error           -- ^ Show error message when something going wrong
  | WordTemplate    -- ^ Send a MS Word template archive
  | OpenTemplate    -- ^ Send a Open Document template archive
  deriving (Show)
                                
-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model
  , botAction = flip handleUpdate
  , botHandler = flip handleAction
  , botJobs = []
  }

-- | Process incoming 'Telegram.Update's and turn them into 'Action's.
handleUpdate :: Model -> Update -> Maybe Action
handleUpdate _ = parseUpdate
    $ Help           <$ (command "start" <|> command "help")
  <|> WordTemplate   <$ (command "word_template" <|> exactText downloadWordTemplateText)
  <|> OpenTemplate   <$ (command "open_template" <|> exactText downloadOpenTemplateText)
  <|> Error          <$ text

-- | How to handle 'Action's.
handleAction ::  Model -> Action -> Eff Action Model
handleAction model NoAction = pure model

handleAction model Error = do
  eff $ do
    reply errorMessage
    pure NoAction
  pure model
  
handleAction model Help = do
  eff $ do
    reply helpMessage
    pure NoAction
  pure model
  
handleAction model WordTemplate = do
  eff $ do
    replyDocumentWithCaption wordTemplateFile (Just templateCaption)
    reply somethingElseMessage
    pure NoAction
  pure model  
  
handleAction model OpenTemplate = do
  eff $ do
    replyDocumentWithCaption openTemplateFile (Just templateCaption)
    reply somethingElseMessage
    pure NoAction
  pure model    

-- | Run bot with a given 'Telegram.Token'.
run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run