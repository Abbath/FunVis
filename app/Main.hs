{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative
import System.IO (IOMode (WriteMode), hPutStrLn, hSetBinaryMode, withFile)
import System.Random.Stateful

data Expr = Param Text | Num Double | Add Expr Expr | Mul Expr Expr | Pow Expr | Fun Text Expr deriving (Show)

showT :: (Show a) => a -> Text
showT = pack . show

prettyPrint :: Expr -> Text
prettyPrint (Param p) = p
prettyPrint (Num x) = showT x
prettyPrint (Add e1 e2) = "(" <> prettyPrint e1 <> " + " <> prettyPrint e2 <> ")"
prettyPrint (Mul e1 e2) = "(" <> prettyPrint e1 <> " * " <> prettyPrint e2 <> ")"
prettyPrint (Pow e1) = "(" <> prettyPrint e1 <> "^2)"
prettyPrint (Fun f e1) = f <> "(" <> prettyPrint e1 <> ")"

generateFunction :: (StatefulGen g m) => Int -> Double -> [Text] -> Weights -> g -> m Expr
generateFunction depth mc ps ws gen
  | depth == 0 = do
      choice <- uniformRM (0 :: Int, 1) gen
      if choice == 0
        then Num <$> randomNumber
        else Param <$> randomChoice ps
  | otherwise = do
      choice <- uniformRM (0.0 :: Double, 1.0) gen
      if
        | choice < w1 ws -> Num <$> randomNumber
        | choice < w2 ws -> Param <$> randomChoice ps
        | choice < w3 ws -> Add <$> gf <*> gf
        | choice < w4 ws -> Mul <$> gf <*> gf
        | choice < w5 ws -> Pow <$> gf
        | otherwise -> Fun <$> randomChoice ["sin", "abs", "sqrt", "log"] <*> gf
 where
  gf = generateFunction (depth - 1) mc ps ws gen
  randomNumber = uniformRM (0 :: Double, mc) gen
  randomChoice v = do
    idx <- uniformRM (0 :: Int, length v - 1) gen
    pure $ v !! idx

computeFunction :: Map Text Double -> Expr -> Double
computeFunction m e = case e of
  Num x -> x
  Param n -> m ! n
  Add e1 e2 -> cf e1 + cf e2
  Mul e1 e2 -> cf e1 * cf e2
  Pow e1 -> cf e1 ** 2
  Fun "sin" e1 -> sin $ cf e1
  Fun "cos" e1 -> cos $ cf e1
  Fun "abs" e1 -> abs $ cf e1
  Fun "sqrt" e1 -> sqrt $ cf e1
  Fun "log" e1 -> log $ cf e1
  _ -> error "Unreachable!"
 where
  cf = computeFunction m

data Rgb = Rgb {r :: Double, g :: Double, b :: Double}

data Output = TextOutput | BinaryOutput deriving (Eq)

data Options = Options
  { maxDepth :: Int
  , fieldSize :: Double
  , imageWidth :: Int
  , imageHeight :: Int
  , maxConstant :: Double
  , weights :: Text
  , output :: Output
  }

options :: Parser Options
options =
  Options
    <$> option auto (long "max-depth" <> short 'd' <> help "Maximum function depth" <> showDefault <> value 25 <> metavar "DEPTH")
    <*> option auto (long "field-size" <> short 's' <> help "Field size" <> showDefault <> value (2 * pi) <> metavar "FSIZE")
    <*> option auto (long "width" <> short 'w' <> help "Width in pixels" <> showDefault <> value 1024 <> metavar "WIDTH")
    <*> option auto (long "height" <> short 'h' <> help "Height in pixels" <> showDefault <> value 1024 <> metavar "HEIGHT")
    <*> option auto (long "max-constant" <> short 'c' <> help "Maximum constant range" <> showDefault <> value 10.0 <> metavar "MAX_CONSTANT")
    <*> strOption (long "weights" <> short 'g' <> help "Weights" <> showDefault <> value "1 1 1 1 1 1" <> metavar "WEIGHTS")
    <*> flag TextOutput BinaryOutput (long "binary-output" <> short 'b')

defaultWeights :: Weights
defaultWeights = Weights 1 1 1 1 1 1

data Weights = Weights
  { w1 :: Double
  , w2 :: Double
  , w3 :: Double
  , w4 :: Double
  , w5 :: Double
  , w6 :: Double
  }

main :: IO ()
main = do
  args <- execParser opts
  gen_r <- newStdGen >>= newIOGenM
  gen_g <- newStdGen >>= newIOGenM
  gen_b <- newStdGen >>= newIOGenM
  let ws = read @[Double] . T.unpack . ("[" <>) . (<> "]") . T.intercalate ", " . T.words $ weights args
  let normWeights = normalizeWieghts (Weights (head ws) (ws !! 1) (ws !! 2) (ws !! 3) (ws !! 4) (ws !! 5))
  fun_r <- generateFunction (maxDepth args) (maxConstant args) ["x", "y"] normWeights gen_r
  T.putStrLn $ prettyPrint fun_r
  fun_g <- generateFunction (maxDepth args) (maxConstant args) ["x", "y"] normWeights gen_g
  T.putStrLn $ prettyPrint fun_g
  fun_b <- generateFunction (maxDepth args) (maxConstant args) ["x", "y"] normWeights gen_b
  T.putStrLn $ prettyPrint fun_b
  let width = imageWidth args
  let height = imageHeight args
  let values =
        map
          ( \n ->
              let (x, y) = n `divMod` width
                  d c s = fromIntegral c / fromIntegral s * fieldSize args - fieldSize args / 2
                  (xd, yd) = (d x width, d y height)
                  cfp = computeFunction [("x", xd), ("y", yd)]
                  v_r = cfp fun_r
                  v_g = cfp fun_g
                  v_b = cfp fun_b
               in Rgb v_r v_g v_b
          )
          ([0 .. width * height - 1] :: [Int])
  let (maxValue_r, minValue_r) = computeBounds r values
  let valueSpan_r = maxValue_r - minValue_r
  let (maxValue_g, minValue_g) = computeBounds g values
  let valueSpan_g = maxValue_g - minValue_g
  let (maxValue_b, minValue_b) = computeBounds b values
  let valueSpan_b = maxValue_b - minValue_b
  withFile "image.ppm" WriteMode $ \h -> do
    hPutStrLn h (if output args /= BinaryOutput then "P3" else "P6")
    hPutStrLn h $ show width <> " " <> show height
    hPutStrLn h "255 "
    when (output args == BinaryOutput) $ hSetBinaryMode h True
    mapM_
      ( \(Rgb v_r v_g v_b) ->
          let c_r = compute v_r minValue_r valueSpan_r
              c_g = compute v_g minValue_g valueSpan_g
              c_b = compute v_b minValue_b valueSpan_b
           in case output args of
                BinaryOutput -> BS.hPut h (BS.pack [fromIntegral c_r, fromIntegral c_g, fromIntegral c_b])
                TextOutput -> hPutStrLn h $ show @Int c_r <> " " <> show @Int c_g <> " " <> show @Int c_b
      )
      values
 where
  compute :: Double -> Double -> Double -> Int
  compute v minValue valueSpan = (`mod` 256) . truncate $ (v - minValue) / valueSpan * 255
  computeBound f g values = f $ g (compare `on` f) values
  computeBounds f values = (computeBound f maximumBy values, computeBound f minimumBy values)
  normalizeWieghts ws =
    let
      s1 = w1 ws
      s2 = w1 ws + w2 ws
      s3 = w1 ws + w2 ws + w3 ws
      s4 = w1 ws + w2 ws + w3 ws + w4 ws
      s5 = w1 ws + w2 ws + w3 ws + w4 ws + w5 ws
      s6 = w1 ws + w2 ws + w3 ws + w4 ws + w5 ws + w6 ws
     in
      Weights (s1 / s6) (s2 / s6) (s3 / s6) (s4 / s6) (s5 / s6) (s6 / s6)
  opts = info (options <**> helper) (fullDesc <> progDesc "Function Visualisation" <> header "Visualise a random function")
