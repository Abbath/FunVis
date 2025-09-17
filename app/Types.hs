{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import Options.Applicative

data Options = Options
  { maxDepth :: Int
  , fieldSize :: Double
  , imageWidth :: Int
  , imageHeight :: Int
  , maxConstant :: Double
  , weights :: Text
  , output :: FilePath
  , funR :: Text
  , funG :: Text
  , funB :: Text
  , funA :: Text
  , attempts :: Int
  , singleFunction :: Bool
  , useAlpha :: Bool
  , step :: Double
  , discord :: Bool
  }
  deriving (Show)

options :: Parser Options
options =
  Options
    <$> option auto (long "max-depth" <> short 'm' <> help "Maximum function depth" <> showDefault <> value 7 <> metavar "DEPTH")
    <*> option auto (long "field-size" <> short 's' <> help "Field size" <> showDefault <> value (2 * pi) <> metavar "FSIZE")
    <*> option auto (long "width" <> short 'w' <> help "Width in pixels" <> showDefault <> value 1024 <> metavar "WIDTH")
    <*> option auto (long "height" <> short 'h' <> help "Height in pixels" <> showDefault <> value 1024 <> metavar "HEIGHT")
    <*> option auto (long "max-constant" <> short 'c' <> help "Maximum constant range" <> showDefault <> value 10.0 <> metavar "MAX_CONSTANT")
    <*> strOption (long "weights" <> short 't' <> help "Weights" <> showDefault <> value "1 1 1 1 1 1 1" <> metavar "WEIGHTS")
    <*> strOption (long "output" <> short 'o' <> help "Output file name" <> showDefault <> value "image.png" <> metavar "OUTPUT")
    <*> strOption (long "red" <> short 'r' <> help "Red channel function" <> value "" <> metavar "RED_FUNCTION")
    <*> strOption (long "green" <> short 'g' <> help "Green channel function" <> value "" <> metavar "GREEN_FUNCTION")
    <*> strOption (long "blue" <> short 'b' <> help "Blue channel function" <> value "" <> metavar "BLUE_FUNCTION")
    <*> strOption (long "alpha" <> short 'a' <> help "Alpha channel function" <> value "" <> metavar "ALPHA_FUNCTION")
    <*> option auto (long "attempts" <> short 'n' <> help "Number of attempts" <> showDefault <> value 0 <> metavar "ATTEMPTS")
    <*> switch (long "single-function" <> short 'l' <> help "Single function")
    <*> switch (long "use-alpha" <> short 'p' <> help "Use alpha")
    <*> option auto (long "step" <> short 'e' <> help "Time step" <> showDefault <> value 0 <> metavar "STEP")
    <*> switch (long "discord" <> short 'd' <> help "Discord bot")
