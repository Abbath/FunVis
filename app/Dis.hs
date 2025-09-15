{-# LANGUAGE OverloadedStrings #-}

module Dis where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text as TS (dropWhile, isPrefixOf, pack, toLower)
import Data.Text.IO as TSIO (putStrLn)
import Data.Text.IO qualified as TIO
import Discord (
  DiscordHandler,
  RunDiscordOpts (discordOnEvent, discordOnLog, discordToken),
  def,
  restCall,
  runDiscord,
 )
import Discord.Requests qualified as R
import Discord.Types (Event (..), Message (messageTimestamp), User (userName), messageAuthor, messageChannelId, messageContent, messageId, userIsBot)
import Performer (generateFunctions, perform)
import System.Environment (lookupEnv)
import Types

-- | Replies "pong" to every message that starts with "ping"
discordImage :: Options -> IO ()
discordImage opts = do
  TIO.putStrLn "Started server"
  loadFile defaultConfig
  token <- fromMaybe "" <$> lookupEnv "DISCORD_TOKEN"
  userFacingError <-
    runDiscord $
      def
        { discordToken = TS.pack token
        , discordOnEvent = eventHandler opts
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        } -- if you see OnLog error, post in the discord / open an issue
  TIO.putStrLn userFacingError

-- userFacingError is an unrecoverable error
-- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Options -> Event -> DiscordHandler ()
eventHandler opts event =
  case event of
    MessageCreate m -> when (isCalc m && not (fromBot m)) $ do
      liftIO . TSIO.putStrLn $ "[" <> (TS.pack . show $ messageTimestamp m) <> "] " <> userName (messageAuthor m) <> ": " <> TS.dropWhile (not . isSpace) (messageContent m)
      void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
      respond m "Ololo"
      funs <- liftIO $ generateFunctions opts
      liftIO $ perform funs opts (-1)
      image_data <- liftIO $ BS.readFile "image.png"
      let mdopts = def @R.MessageDetailedOpts
      void . restCall $
        R.CreateMessageDetailed (messageChannelId m) $
          mdopts
            { R.messageDetailedFile =
                Just
                  ("image.png", image_data)
            }
    _ -> return ()
 where
  respond m rsp =
    void $
      restCall
        ( R.CreateMessage
            (messageChannelId m)
            rsp
        )

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCalc :: Message -> Bool
isCalc = (\m -> "!image" `TS.isPrefixOf` m) . TS.toLower . messageContent
