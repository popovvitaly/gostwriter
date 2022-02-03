{-# LANGUAGE OverloadedStrings #-}
-- | This bot replies "Got it" to every incoming update.
module Main where

import           Control.Applicative              ((<|>))

import           Data.Text                 (Text, unlines)

import qualified Telegram.Bot.API          as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser (parseUpdate, command, text)

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | Help        -- ^ Show help message and go to empty state 
  | Error       -- ^ Show error message when something going wrong
  deriving (Show)
  
-- | A help message to show on conversation start with bot or when user promts /help
helpMessage :: Text
helpMessage = Data.Text.unlines
 [ "Привет! Я бот, который может помочь тебе оформить курсовую или дипломную работу согласно всем требованиям."
 , ""
 , "- Напиши /template для того, чтобы получить шаблон с готовыми стилями для Word. Инструкция — внутри!"
 , ""
 , "- Чтобы увидеть это сообщение езе раз, напиши /help"
 , ""
 , "Если у тебя есть замечания, предложения или желание улучшить функционал данного бота учавствуя непосредственно в разработке: https://github.com/popovvitaly/gostwriter"
 , ""
 , "Итак, с чего начнем?"
 ]  
 
-- | A error message to show it when something going wrong
errorMessage :: Text
errorMessage = Data.Text.unlines
 [ "Ой! Я тебя не понял или что-то пошло не так."
 , ""
 , "Если ты считаешь, что этого не должно было произойти, то можешь сообщить подробности об ошибке или или даже исправить самостоятельно по ссылке: https://github.com/popovvitaly/gostwriter"
 , ""
 , "Если хочешь увидеть список доступных команд, напиши /help"
 ]                                   

-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model
  , botAction = flip handleUpdate
  , botHandler = flip handleAction
  , botJobs = []
  }

-- | Process incoming 'Telegram.Update's and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
    $ Help       <$ (command "start" <|> command "help")
  <|> Error      <$ text

-- | How to handle 'Action's.
handleAction ::  Model -> Action -> Eff Action Model
handleAction model NoAction = pure model

handleAction model Error = do
  eff $ do
    reply (toReplyMessage errorMessage)
      { replyMessageDisableWebPagePreview = Just True
      }
    pure NoAction
  pure model
  
handleAction model Help = do
  eff $ do
    reply (toReplyMessage helpMessage)
      { replyMessageDisableWebPagePreview = Just True
      }
    pure NoAction
  pure model

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
