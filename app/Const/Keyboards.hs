module Const.Keyboards(
  emptyStateMessageKeyboard
) where

import Telegram.Bot.API.Types
import Const.Texts

-- | Keyboard with all basic bot actions that will show at startup
emptyStateMessageKeyboard :: ReplyKeyboardMarkup
emptyStateMessageKeyboard = ReplyKeyboardMarkup
  { replyKeyboardMarkupKeyboard = [ [downloadWordTemplateButton, downloadOpenTemplateButton] ]
  , replyKeyboardMarkupResizeKeyboard = Just True
  , replyKeyboardMarkupOneTimeKeyboard = Just True
  , replyKeyboardMarkupSelective = Just True
  , replyKeyboardMarkupInputFieldSelector = Nothing
  }

downloadOpenTemplateButton :: KeyboardButton
downloadOpenTemplateButton = KeyboardButton
  { keyboardButtonText = downloadOpenTemplateText
  , keyboardButtonRequestContact = Just False
  , keyboardButtonRequestLocation = Just False
  , keyboardButtonRequestPoll = Nothing
  }

downloadWordTemplateButton :: KeyboardButton
downloadWordTemplateButton = KeyboardButton
  { keyboardButtonText = downloadWordTemplateText
  , keyboardButtonRequestContact = Just False
  , keyboardButtonRequestLocation = Just False
  , keyboardButtonRequestPoll = Nothing
  }
