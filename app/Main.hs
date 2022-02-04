{-# LANGUAGE OverloadedStrings #-}
-- | This bot replies "Got it" to every incoming update.
module Main where

import Control.Applicative              ((<|>))

import Data.Text                        (Text, unlines)

import ParserExtensions                 (exactText)
import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.Debug
import Telegram.Bot.Simple.UpdateParser (parseUpdate, command, text)
import Control.Monad.Reader

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
  
-- | A help message to show on conversation start with bot or when user promts /help
helpMessage :: Text
helpMessage = Data.Text.unlines
 [ "Привет! Я бот, который может помочь тебе оформить курсовую или дипломную работу согласно всем требованиям."
 , ""
 , "- Напиши /word_template для того, чтобы получить .docx шаблон с готовыми стилями для MS Word."
 , ""
 , "- Напиши /open_template для того, чтобы получить .odt шаблон с готовыми стилями для OpenOffice, LibreOffice и др."
 , ""
 , "- Чтобы увидеть это сообщение еще раз, напиши /help"
 , ""
 , "Если у тебя есть замечания, предложения или желание улучшить функционал данного бота участвуя непосредственно в разработке: https://github.com/popovvitaly/gostwriter"
 , ""
 , "Итак, с чего начнем?"
 ]  
 
-- | Keyboard with all basic bot actions that will show at startup 
emptyStateMessageKeyboard :: ReplyKeyboardMarkup
emptyStateMessageKeyboard = ReplyKeyboardMarkup
  { replyKeyboardMarkupKeyboard = [ ["Скачать .docx шаблон", "Скачать .odt шаблон"] ] 
  , replyKeyboardMarkupResizeKeyboard = Just True
  , replyKeyboardMarkupOneTimeKeyboard = Just True
  , replyKeyboardMarkupSelective = Just True
  , replyKeyboardMarkupInputFieldSelector = Just "Команда"
  }
 
-- | A error message to show it when something going wrong
errorMessage :: Text
errorMessage = Data.Text.unlines
 [ "Ой! Я тебя не понял или что-то пошло не так."
 , ""
 , "Если ты считаешь, что этого не должно было произойти, то можешь сообщить подробности об ошибке или или даже исправить самостоятельно по ссылке: https://github.com/popovvitaly/gostwriter"
 , ""
 , "Если хочешь увидеть список доступных команд, напиши /help"
 ]    
 
somethingElseMessage :: Text 
somethingElseMessage = "Что-нибудь еще?"  
 
-- | A pre-uploaded MS Word template file todo change file id to not fake file id
wordTemplateFile :: DocumentFile
wordTemplateFile = DocumentFileId $ FileId "BQACAgIAAxkBAAM3YfvPiXUAAYCEpyiqqci0_7MJEurTAAL1FAACwivgS0nDgO9bfmFCIwQ"
        
-- | A pre-uploaded open document template file todo change file id to not fake file id
openTemplateFile :: DocumentFile
openTemplateFile = DocumentFileId $ FileId "BQACAgIAAxkBAANOYfzkKCGbuJz8eDyY4kbgW1iDkvEAApsVAAIhjOlL_TZmSqsdytgjBA"
                                
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
  <|> WordTemplate   <$ (command "word_template" <|> exactText "Скачать .docx шаблон")
  <|> OpenTemplate   <$ (command "open_template" <|> exactText "Скачать .odt шаблон")
  <|> Error          <$ text

-- | How to handle 'Action's.
handleAction ::  Model -> Action -> Eff Action Model
handleAction model NoAction = pure model

handleAction model Error = do
  eff $ do
    reply (toReplyMessage errorMessage)
      { replyMessageDisableWebPagePreview = Just True
      , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup emptyStateMessageKeyboard
      }
    pure NoAction
  pure model
  
handleAction model Help = do
  eff $ do
    reply (toReplyMessage helpMessage)
      { replyMessageDisableWebPagePreview = Just True
      , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup emptyStateMessageKeyboard
      }
    pure NoAction
  pure model
  
handleAction model WordTemplate = do
  eff $ do
    mchatId <- currentChatId
    case mchatId of
      Just chatId' -> void . liftClientM . sendDocument $ (toSendDocument (SomeChatId chatId') wordTemplateFile) 
        { sendDocumentCaption = Just "Внутри готовые стили, инструкции и примеры использования"
        }
      Nothing     -> liftIO $ putStrLn "No such chat with same chat id"
    reply (toReplyMessage somethingElseMessage)
      { replyMessageDisableWebPagePreview = Just True
      , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup emptyStateMessageKeyboard
      }   
    pure NoAction
  pure model  
  
handleAction model OpenTemplate = do
  eff $ do
    mchatId <- currentChatId
    case mchatId of
      Just chatId' -> void . liftClientM . sendDocument $ (toSendDocument (SomeChatId chatId') openTemplateFile) 
        { sendDocumentCaption = Just "Внутри готовые стили, инструкции и примеры использования"
        }
      Nothing     -> liftIO $ putStrLn "No such chat with same chat id"
    reply (toReplyMessage somethingElseMessage)
      { replyMessageDisableWebPagePreview = Just True
      , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup emptyStateMessageKeyboard
      }        
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