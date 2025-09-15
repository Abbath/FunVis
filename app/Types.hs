module Types where

import Data.Text (Text)

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
