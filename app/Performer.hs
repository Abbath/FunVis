{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Performer (generateFunctions, perform) where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.Massiv.Array (Ix2 ((:.)), IxN ((:>)), (!>), (.-), (./))
import Data.Massiv.Array qualified as M
import Data.Massiv.Array.IO qualified as MIO
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Word
import ExprParser (Cond (..), Expr (..), FunType (..), Parameter (..), parseExpr)
import Graphics.ColorModel qualified as CM
import Graphics.Pixel.ColorSpace qualified as C
import System.FilePath (replaceBaseName, takeBaseName, (-<.>))
import System.Random.Stateful
import Text.Megaparsec.Error
import Text.Printf
import Types

newtype Weights = Weights (V.Vector Double)

generateFunction :: (StatefulGen g m) => Int -> Double -> V.Vector Parameter -> Weights -> g -> m Expr
generateFunction depth mc ps ws@(Weights w) gen
  | depth == 0 = do
      choice <- uniformRM (0 :: Int, 1) gen
      if choice == 0
        then Num <$> randomNumber
        else Param <$> randomChoice ps
  | otherwise = do
      choice <- uniformRM (0.0 :: Double, 1.0) gen
      if
        | choice < w V.! 0 -> Num <$> randomNumber
        | choice < w V.! 1 -> Param <$> randomChoice ps
        | choice < w V.! 2 -> Add <$> gf <*> gf
        | choice < w V.! 3 -> Mul <$> gf <*> gf
        | choice < w V.! 4 -> Pow <$> randomChoice [2.0, 3.0] <*> gf
        | choice < w V.! 5 -> If <$> randomChoice [GreaterEqual, Less] <*> gf <*> gf <*> gf <*> gf
        | otherwise -> Fun <$> randomChoice [Sin, Abs, Sqrt, Log, Inv] <*> gf
 where
  gf = generateFunction (depth - 1) mc ps ws gen
  randomNumber = uniformRM (-mc, mc) gen
  randomChoice v = (v V.!) <$> uniformRM (0 :: Int, length v - 1) gen

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
  Param p -> T.pack $ toLower <$> show p
  Num x -> showT x
  Add e1 e2 -> "(" <> pp e1 <> " + " <> pp e2 <> ")"
  Mul (Num (-1)) e2 -> "(-" <> pp e2 <> ")"
  Mul e1 e2 -> "(" <> pp e1 <> " * " <> pp e2 <> ")"
  Pow n e1 -> "(" <> pp e1 <> "^" <> showT n <> ")"
  Fun f e1 -> prettyPrintFun f <> "(" <> pp e1 <> ")"
  If cond a b c d -> "if(" <> pp a <> (if cond == GreaterEqual then " >= " else " < ") <> pp b <> ", " <> pp c <> ", " <> pp d <> ")"
 where
  pp = prettyPrint

testFunction :: Expr -> Bool
testFunction = \case
  Param _ -> True
  Num _ -> False
  Add e1 e2 -> tf e1 || tf e2
  Mul e1 e2 -> tf e1 || tf e2
  Pow _ e1 -> tf e1
  Fun _ e1 -> tf e1
  If _ e1 e2 e3 e4 -> (tf e1 || tf e2) && (tf e3 || tf e4)
 where
  tf = testFunction

parseFunction :: Text -> Expr
parseFunction f = case parseExpr (T.unpack f) of
  Left err -> error (errorBundlePretty err)
  Right ast -> ast

generateFunctionWrapper :: (StatefulGen g m) => Int -> Double -> V.Vector Parameter -> Weights -> g -> m Expr
generateFunctionWrapper md mc ps ws g = do
  fun <- generateFunction md mc ps ws g
  if testFunction fun
    then pure fun
    else generateFunctionWrapper md mc ps ws g

genFun :: (Applicative f) => Text -> f Expr -> f Expr
genFun "" g = g
genFun f _ = pure . parseFunction $ f

normalizeWeights :: Weights -> Weights
normalizeWeights (Weights ws) =
  let sums = V.scanl1 (+) ws
      d = V.last sums
   in Weights $ (/ d) <$> sums

generateFunctions :: Options -> IO (V.Vector Expr)
generateFunctions args = do
  gen <- newStdGen >>= newIOGenM
  let ws = read @(V.Vector Double) . T.unpack . ("[" <>) . (<> "]") . T.intercalate ", " . T.words $ weights args
  let normWeights = normalizeWeights $ Weights ws
  let gf = generateFunctionWrapper (maxDepth args) (maxConstant args) [X, Y, T] normWeights
  let pf f = T.putStrLn (prettyPrint f) >> T.putStrLn ""
  fun_r <- genFun (funR args) (gf gen)
  pf fun_r
  fun_g <- genFun (funG args) (gf gen)
  unless (singleFunction args) $ pf fun_g
  fun_b <- genFun (funB args) (gf gen)
  unless (singleFunction args) $ pf fun_b
  fun_a <- if useAlpha args then genFun (funA args) (gf gen) else pure $ Num 1
  unless (singleFunction args) $ pf fun_a
  let funs = if singleFunction args then [fun_r, fun_r, fun_r, Num 1] else [fun_r, fun_g, fun_b, fun_a] :: V.Vector Expr
  pure funs

computeFunction :: (Double, Double, Double) -> Expr -> Double
computeFunction m@(px, py, pt) = \case
  Num x -> x
  Param X -> px
  Param Y -> py
  Param T -> pt
  Add e1 e2 -> cf e1 + cf e2
  Mul e1 e2 -> cf e1 * cf e2
  Pow n e1 -> cf e1 ** n
  Fun Sin e1 -> sin $ cf e1
  Fun Abs e1 -> abs $ cf e1
  Fun Sqrt e1 -> sqrt $ cf e1
  Fun Log e1 -> log $ cf e1
  Fun Inv e1 -> let d = cf e1 in if d == 0 then d else 1 / cf e1
  If cond a b c d ->
    cf $
      if case cond of
        GreaterEqual -> cf a >= cf b
        Less -> cf a < cf b
        then c
        else d
 where
  cf = computeFunction m
{-# INLINE computeFunction #-}

perform :: V.Vector Expr -> Options -> Int -> IO ()
perform funs args idx = do
  let !width = imageWidth args
  let !height = imageHeight args
  let !fs = fieldSize args
  let d c s = fromIntegral c / fromIntegral s * fs - fs / 2
  let t = if idx == -1 then 0.0 else fromIntegral idx * step args
  let massive_values = M.makeArray @M.D M.Par (M.Sz (4 :> height :. width)) (\(k :> i :. j) -> computeFunction (d i height, d j width, t) (funs V.! k))
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
