module Const.Texts where

import Data.Text (Text, unlines, pack)

-- | todo change file id to not fake file id
msWordTemplateFileId :: [Char]
msWordTemplateFileId = "BQACAgIAAxkBAAM3YfvPiXUAAYCEpyiqqci0_7MJEurTAAL1FAACwivgS0nDgO9bfmFCIwQ"

-- | todo change file id to not fake file id
openTemplateFileId :: [Char]
openTemplateFileId = "BQACAgIAAxkBAANOYfzkKCGbuJz8eDyY4kbgW1iDkvEAApsVAAIhjOlL_TZmSqsdytgjBA"
 
helpMessageText :: Text
helpMessageText = Data.Text.unlines $ map pack 
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
                   
errorMessageText :: Text
errorMessageText = Data.Text.unlines $ map pack [ "Ой! Я тебя не понял или что-то пошло не так."
                    , ""
                    , "Если ты считаешь, что этого не должно было произойти, то можешь сообщить подробности об ошибке или или даже исправить самостоятельно по ссылке: https://github.com/popovvitaly/gostwriter"
                    , ""
                    , "Если хочешь увидеть список доступных команд, напиши /help"
                    ]   
                    
somethingElseMessageText :: Text
somethingElseMessageText = pack "Что-нибудь еще?"                         

downloadWordTemplateText :: Text
downloadWordTemplateText = pack "Скачать .docx шаблон"   

downloadOpenTemplateText :: Text
downloadOpenTemplateText = pack "Скачать .odt шаблон"

templateCaption :: Text
templateCaption = pack "Внутри готовые стили, инструкции и примеры использования"       