{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (throwIO)
import Control.Lens ((.~), (^.))
import Control.Monad (forever, void)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.String (fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wreq (defaults, getWith, param, responseBody)
import Servant.Client (ClientEnv, ClientM, mkClientEnv, runClientM)
import System.Environment (getEnv)
import Telegram.Bot.API qualified as Api
import Time (minute, threadDelay)

makePost :: Text -> Bot -> IO ()
makePost txt Bot{channelId, clientEnv} = do
  void $
    runTelegramClient clientEnv $
      Api.sendMessage $
        Api.defSendMessage channelId txt

newtype FishTextResponse = FishTextResponse {text :: Text}
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show)

data Bot = Bot {channelId :: Api.SomeChatId, clientEnv :: ClientEnv}

getPostText :: IO Text
getPostText = do
  resp <-
    getWith (defaults & param "number" .~ ["1"]) "https://fish-text.ru/get"
  FishTextResponse{text} <- Aeson.throwDecode $ resp ^. responseBody
  pure text

main :: IO ()
main = do
  loadFile defaultConfig
  bot <- newBot
  forever do
    txt <- getPostText
    makePost txt bot
    threadDelay $ minute 5

newBot :: IO Bot
newBot = do
  token <- fromString <$> getEnv "TELEGRAM_BOT_TOKEN"
  channelId <-
    Api.SomeChatId . Api.ChatId . read <$> getEnv "TELEGRAM_BOT_CHANNEL_ID"
  httpManager <- newTlsManager
  pure
    Bot{channelId, clientEnv = mkClientEnv httpManager $ Api.botBaseUrl token}

runTelegramClient :: ClientEnv -> ClientM a -> IO a
runTelegramClient clientEnv action = do
  eResult <- runClientM action clientEnv
  either throwIO pure eResult
