{-# LANGUAGE OverloadedStrings #-}

module Dis (discordImage) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
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
import Options.Applicative
import Performer (generateFunctions, perform)
import System.Environment (lookupEnv)
import Types

discordImage :: IO ()
discordImage = do
  TIO.putStrLn "Started server"
  loadFile defaultConfig
  token <- fromMaybe "" <$> lookupEnv "DISCORD_TOKEN"
  userFacingError <-
    runDiscord $
      def
        { discordToken = T.pack token
        , discordOnEvent = eventHandler
        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        } -- if you see OnLog error, post in the discord / open an issue
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event =
  case event of
    MessageCreate m | isImage m && not (fromBot m) -> do
      let cid = messageChannelId m
      let maybe_opts = getParseResult . execParserPure defaultPrefs (info (options <**> helper) fullDesc) . parseCommands . T.drop 6 . messageContent $ m
      case maybe_opts of
        Just opts -> do
          liftIO . TIO.putStrLn $ "[" <> (T.pack . show $ messageTimestamp m) <> "] " <> userName (messageAuthor m) <> ": " <> T.dropWhile (not . isSpace) (messageContent m)
          vrest $ R.CreateReaction (cid, messageId m) "eyes"
          respond m "Ololo"
          liftIO $ generateFunctions opts >>= \funs -> perform funs opts (-1)
          image_data <- liftIO $ BS.readFile "image.png"
          vrest $ R.CreateMessageDetailed cid $ (def @R.MessageDetailedOpts){R.messageDetailedFile = Just ("image.png", image_data)}
        Nothing -> respond m "Wrong options"
    _ -> pure ()
 where
  vrest act = void $ restCall act
  respond m rsp = vrest $ R.CreateMessage (messageChannelId m) rsp
  fromBot = userIsBot . messageAuthor
  isImage = (\m -> "!image" `T.isPrefixOf` m) . T.toLower . messageContent
  parseCommands m = case T.uncons m of
    Just (h, t) | h `elem` [' ', '"'] -> let (x, xs) = T.breakOn (T.singleton h) t in T.unpack x : parseCommands xs
    _ -> []
