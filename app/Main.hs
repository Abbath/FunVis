{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, unless)
import Data.Massiv.Array (Ix2 ((:.)), IxN ((:>)), (!>), (.-), (./))
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO qualified as MIO
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Word
import ExprParser (Cond (..), Expr (..), FunType (..), parseExpr)
import Graphics.ColorModel qualified as CM
import Graphics.Pixel.ColorSpace qualified as C
import Options.Applicative
import System.FilePath (replaceBaseName, takeBaseName, (-<.>))
import System.Random.Stateful
import Text.Megaparsec.Error
import Text.Printf

showT :: (Show a) => a -> Text
showT = pack . show

prettyPrintFun :: FunType -> Text
prettyPrintFun = \case
  Sin -> "sin"
  Abs -> "abs"
  Sqrt -> "sqrt"
  Log -> "log"
  Inv -> "inv"

prettyPrint :: Expr -> Text
prettyPrint = \case
  Param p -> p
  Num x -> showT x
  Add e1 e2 -> "(" <> prettyPrint e1 <> " + " <> prettyPrint e2 <> ")"
  Mul (Num (-1)) e2 -> "(-" <> prettyPrint e2 <> ")"
  Mul e1 e2 -> "(" <> prettyPrint e1 <> " * " <> prettyPrint e2 <> ")"
  Pow n e1 -> "(" <> prettyPrint e1 <> "^" <> showT n <> ")"
  Fun f e1 -> prettyPrintFun f <> "(" <> prettyPrint e1 <> ")"
  If cond a b c d -> "if(" <> prettyPrint a <> (if cond == GreaterEqual then " >= " else " < ") <> prettyPrint b <> ", " <> prettyPrint c <> ", " <> prettyPrint d <> ")"

testFunction :: Expr -> Bool
testFunction = \case
  Param _ -> True
  Num _ -> False
  Add e1 e2 -> testFunction e1 || testFunction e2
  Mul e1 e2 -> testFunction e1 || testFunction e2
  Pow _ e1 -> testFunction e1
  Fun _ e1 -> testFunction e1
  If _ e1 e2 e3 e4 -> (testFunction e1 || testFunction e2) && (testFunction e3 || testFunction e4)

generateFunction :: (StatefulGen g m) => Int -> Double -> V.Vector Text -> Weights -> g -> m Expr
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
        | choice < w5 ws -> Pow <$> randomChoice [2.0, 3.0] <*> gf
        | choice < w6 ws -> If <$> randomChoice [GreaterEqual, Less] <*> gf <*> gf <*> gf <*> gf
        | otherwise -> Fun <$> randomChoice [Sin, Abs, Sqrt, Log, Inv] <*> gf
 where
  gf = generateFunction (depth - 1) mc ps ws gen
  randomNumber = uniformRM (-mc, mc) gen
  randomChoice v = do
    idx <- uniformRM (0 :: Int, length v - 1) gen
    pure $ v V.! idx

computeFunction :: (Double, Double, Double) -> Expr -> Double
computeFunction m@(px, py, pt) = \case
  Num x -> x
  Param "x" -> px
  Param "y" -> py
  Param "t" -> pt
  Add e1 e2 -> cf e1 + cf e2
  Mul e1 e2 -> cf e1 * cf e2
  Pow n e1 -> cf e1 ** n
  Fun Sin e1 -> sin $ cf e1
  Fun Abs e1 -> abs $ cf e1
  Fun Sqrt e1 -> sqrt $ cf e1
  Fun Log e1 -> log $ cf e1
  Fun Inv e1 -> let d = cf e1 in if d == 0 then d else 1 / cf e1
  If cond a b c d ->
    if case cond of
      GreaterEqual -> cf a >= cf b
      Less -> cf a < cf b
      then cf c
      else cf d
  _ -> error "Unreachable!"
 where
  cf = computeFunction m
{-# INLINE computeFunction #-}

data Output = PngOutput | TextOutput | BinaryOutput deriving (Eq, Show)

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
  }

options :: Parser Options
options =
  Options
    <$> option auto (long "max-depth" <> short 'd' <> help "Maximum function depth" <> showDefault <> value 25 <> metavar "DEPTH")
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

data Weights = Weights
  { w1 :: Double
  , w2 :: Double
  , w3 :: Double
  , w4 :: Double
  , w5 :: Double
  , w6 :: Double
  , w7 :: Double
  }

parseFunction :: Text -> Expr
parseFunction f = case parseExpr (T.unpack f) of
  Left err -> error (errorBundlePretty err)
  Right ast -> ast

main :: IO ()
main = do
  args <- execParser opts
  case attempts args of
    0 -> perform args (-1)
    n -> forM_ ([0 .. n - 1] :: [Int]) $ perform args
 where
  opts = info (options <**> helper) (fullDesc <> progDesc "Function Visualisation" <> header "Visualise a random function")

generateFunctionWrapper :: (StatefulGen g m) => Int -> Double -> V.Vector Text -> Weights -> g -> m Expr
generateFunctionWrapper md mc ps ws g = do
  fun <- generateFunction md mc ps ws g
  if testFunction fun
    then pure fun
    else generateFunctionWrapper md mc ps ws g

genFun :: (Applicative f) => Text -> f Expr -> f Expr
genFun f g =
  if f == ""
    then g
    else pure . parseFunction $ f

perform :: Options -> Int -> IO ()
perform args idx = do
  gen_r <- newStdGen >>= newIOGenM
  gen_g <- newStdGen >>= newIOGenM
  gen_b <- newStdGen >>= newIOGenM
  gen_a <- newStdGen >>= newIOGenM
  let ws = read @[Double] . T.unpack . ("[" <>) . (<> "]") . T.intercalate ", " . T.words $ weights args
  let normWeights = normalizeWieghts (Weights (head ws) (ws !! 1) (ws !! 2) (ws !! 3) (ws !! 4) (ws !! 5) (ws !! 6))
  let gf = generateFunctionWrapper (maxDepth args) (maxConstant args) ["x", "y", "t"] normWeights
  let pf f = T.putStrLn (prettyPrint f) >> T.putStrLn ""
  fun_r <- genFun (funR args) (gf gen_r)
  pf fun_r
  fun_g <- genFun (funG args) (gf gen_g)
  unless (singleFunction args) $ pf fun_g
  fun_b <- genFun (funB args) (gf gen_b)
  unless (singleFunction args) $ pf fun_b
  fun_a <- genFun (funA args) (gf gen_a)
  unless (singleFunction args) $ pf fun_a
  let !width = imageWidth args
  let !height = imageHeight args
  let !fs = fieldSize args
  let d c s = fromIntegral c / fromIntegral s * fs - fs / 2
  let funs = if singleFunction args then [fun_r, fun_r, fun_r, Num 1] else [fun_r, fun_g, fun_b, fun_a] :: V.Vector Expr
  let t = if idx == -1 then 0.0 else fromIntegral idx * 0.1
  let massive_values = M.makeArray @M.D M.Par (M.Sz (4 :> width :. height)) (\(k :> i :. j) -> computeFunction (d i width, d j height, t) (funs V.! k))
  let (max_r, min_r) = computeBounds $ massive_values !> 0
  let (max_g, min_g) = computeBounds $ massive_values !> 1
  let (max_b, min_b) = computeBounds $ massive_values !> 2
  let (max_a, min_a) = computeBounds $ massive_values !> 3
  let span_r = (max_r - min_r) / 255
  let span_g = (max_g - min_g) / 255
  let span_b = (max_b - min_b) / 255
  let span_a = (max_a - min_a) / 255
  let arr_r = ((massive_values !> 0) .- min_r) ./ span_r
  let arr_g = ((massive_values !> 1) .- min_g) ./ span_g
  let arr_b = ((massive_values !> 2) .- min_b) ./ span_b
  let arr_a = ((massive_values !> 3) .- min_a) ./ span_a
  let arr = M.zipWith4 (\r g b a -> C.Pixel $ CM.ColorRGBA (truncate r) (truncate g) (truncate b) (255 - truncate a)) arr_r arr_g arr_b arr_a :: MIO.Image M.D (C.Alpha CM.RGB) Word8
  let filename =
        if idx == -1
          then output args
          else let fname = output args in replaceBaseName fname (takeBaseName fname <> "_" <> printf "%03d" idx)
  MIO.writeImage (filename -<.> "png") arr
 where
  computeBounds values = (M.maximum' values, M.minimum' values)
  normalizeWieghts ws =
    let
      s7 = w1 ws + w2 ws + w3 ws + w4 ws + w5 ws + w6 ws + w7 ws
      s6 = s7 - w7 ws
      s5 = s6 - w6 ws
      s4 = s5 - w5 ws
      s3 = s4 - w4 ws
      s2 = s3 - w3 ws
      s1 = s2 - w2 ws
      d = s7
     in
      Weights (s1 / d) (s2 / d) (s3 / d) (s4 / d) (s5 / d) (s6 / d) (s7 / d)
