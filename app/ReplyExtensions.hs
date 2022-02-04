module ReplyExtensions(
  replyDocumentWithCaption
) where

import Telegram.Bot.Simple.Reply
import Telegram.Bot.Simple.Eff (BotM, liftClientM)
import Telegram.Bot.API.Methods
import Data.Text (Text)
import Telegram.Bot.API.Types
import Control.Monad.Cont (liftIO, void)    
    
-- | Reply with document in the current chat (if possible).    
replyDocumentWithCaption :: DocumentFile -> Maybe Text -> BotM()
replyDocumentWithCaption document caption = do
  mchatId <- currentChatId
  case mchatId of
    Just chatId' -> void . liftClientM . sendDocument $ (toSendDocument (SomeChatId chatId') document) 
      { sendDocumentCaption = caption 
      }      
    Nothing     -> liftIO $ putStrLn "No such chat with same chat id"    