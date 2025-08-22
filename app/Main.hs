{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Text (Text, pack)
import Data.Text.IO qualified as T
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import System.Random.Stateful

data Expr = Param Text | Num Double | Add Expr Expr | Mul Expr Expr | Pow Expr | Fun Text Expr deriving (Show)

showT :: (Show a) => a -> Text
showT = pack . show

prettyPrint :: Expr -> Text
prettyPrint (Param p) = p
prettyPrint (Num x) = showT x
prettyPrint (Add e1 e2) = "(" <> prettyPrint e1 <> " + " <> prettyPrint e2 <> ")"
prettyPrint (Mul e1 e2) = "(" <> prettyPrint e1 <> " * " <> prettyPrint e2 <> ")"
prettyPrint (Pow e1) = prettyPrint e1 <> "^2"
prettyPrint (Fun f e1) = f <> "(" <> prettyPrint e1 <> ")"

generateFunction :: (StatefulGen g m) => Int -> [Text] -> g -> m Expr
generateFunction 0 ps gen = do
  choice <- uniformRM (0 :: Int, 1) gen
  if choice == 0
    then uniformRM (0 :: Double, 10) gen >>= pure <$> Num
    else uniformRM (0, length ps - 1) gen >>= pure <$> Param . (ps !!)
generateFunction depth ps gen = do
  choice <- uniformRM (1 :: Int, 6) gen
  case choice of
    1 -> do
      arg <- uniformRM (0 :: Double, 10) gen
      pure $ Num arg
    2 -> do
      idx <- uniformRM (0, length ps - 1) gen
      pure . Param . (!! idx) $ ps
    3 -> Add <$> gf <*> gf
    4 -> Mul <$> gf <*> gf
    5 -> Pow <$> gf
    6 -> do
      idx <- uniformRM (0 :: Int, 2) gen
      Fun (["sin", "cos", "abs"] !! idx) <$> gf
    _ -> error "Unreachable!"
 where
  gf = generateFunction (depth - 1) ps gen

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
  _ -> error "Unreachable!"
 where
  cf = computeFunction m

data Rgb = Rgb {r :: Double, g :: Double, b :: Double}

maxFunDepth :: Int
maxFunDepth = 15

main :: IO ()
main = do
  gen_r <- newStdGen >>= newIOGenM
  gen_g <- newStdGen >>= newIOGenM
  gen_b <- newStdGen >>= newIOGenM
  fun_r <- generateFunction maxFunDepth ["x", "y"] gen_r
  T.putStrLn $ prettyPrint fun_r
  fun_g <- generateFunction maxFunDepth ["x", "y"] gen_g
  T.putStrLn $ prettyPrint fun_g
  fun_b <- generateFunction maxFunDepth ["x", "y"] gen_b
  T.putStrLn $ prettyPrint fun_b
  let size = 1000 :: Int
  let values =
        map
          ( \n ->
              let (x, y) = n `divMod` size
                  xd = fromIntegral x / fromIntegral size * 2 * pi
                  yd = fromIntegral y / fromIntegral size * 2 * pi
                  v_r = computeFunction [("x", xd), ("y", yd)] fun_r
                  v_g = computeFunction [("x", xd), ("y", yd)] fun_g
                  v_b = computeFunction [("x", xd), ("y", yd)] fun_b
               in (Rgb v_r v_g v_b, n)
          )
          ([0 .. size * size - 1] :: [Int])
  let maxValue_r = r . fst $ maximumBy (compare `on` r . fst) values
  let minValue_r = r . fst $ minimumBy (compare `on` r . fst) values
  let valueSpan_r = maxValue_r - minValue_r
  let maxValue_g = g . fst $ maximumBy (compare `on` g . fst) values
  let minValue_g = g . fst $ minimumBy (compare `on` g . fst) values
  let valueSpan_g = maxValue_g - minValue_g
  let maxValue_b = b . fst $ maximumBy (compare `on` b . fst) values
  let minValue_b = b . fst $ minimumBy (compare `on` b . fst) values
  let valueSpan_b = maxValue_b - minValue_b
  withFile "image.ppm" WriteMode $ \h -> do
    hPutStrLn h "P3"
    hPutStrLn h $ show size <> " " <> show size
    hPutStrLn h "255"
    mapM_
      ( \(Rgb v_r v_g v_b, n) ->
          let c_r = (`mod` 256) . truncate $ (v_r - minValue_r) / valueSpan_r * 255
              c_g = (`mod` 256) . truncate $ (v_g - minValue_g) / valueSpan_g * 255
              c_b = (`mod` 256) . truncate $ (v_b - minValue_b) / valueSpan_b * 255
           in do
                hPutStrLn h $ show @Int c_r <> " " <> show @Int c_g <> " " <> show @Int c_b
                when (n `mod` 3 == 2) $ hPutStrLn h ""
      )
      values
