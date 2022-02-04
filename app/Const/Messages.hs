module Const.Messages(
  openTemplateFile
, wordTemplateFile
, helpMessage
, errorMessage
, somethingElseMessage
) where

import Const.Texts
import Const.Keyboards

import Telegram.Bot.API.Types
import Telegram.Bot.API.Methods
import Telegram.Bot.Simple.Reply

import Data.Text (pack)

-- | A pre-uploaded MS Word template file 
wordTemplateFile :: DocumentFile
wordTemplateFile = DocumentFileId . FileId $ pack msWordTemplateFileId
        
-- | A pre-uploaded open document template file
openTemplateFile :: DocumentFile
openTemplateFile = DocumentFileId . FileId $ pack openTemplateFileId

-- | A help message to show on conversation start with bot or when user promts /help
helpMessage :: ReplyMessage
helpMessage =  (toReplyMessage helpMessageText)
      { replyMessageDisableWebPagePreview = Just True
      , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup emptyStateMessageKeyboard
      }

-- | A error message to show it when something going wrong
errorMessage :: ReplyMessage
errorMessage = (toReplyMessage errorMessageText)
      { replyMessageDisableWebPagePreview = Just True
      , replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup emptyStateMessageKeyboard
      }

-- | A message showing when any operation has been done
somethingElseMessage :: ReplyMessage
somethingElseMessage = (toReplyMessage somethingElseMessageText)
    { replyMessageReplyMarkup = Just $ SomeReplyKeyboardMarkup emptyStateMessageKeyboard
    }