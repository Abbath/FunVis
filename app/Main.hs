{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Text (Text)
import System.IO (IOMode (WriteMode), hPutStr, hPutStrLn, withFile)
import System.Random.Stateful

data Expr = Param Text | Num Double | Add Expr Expr | Mul Expr Expr deriving (Show)

generateFunction :: (StatefulGen g m) => [Text] -> g -> m Expr
generateFunction ps gen = do
  choice <- uniformRM (1 :: Int, 4) gen
  case choice of
    1 -> do
      arg <- uniformRM (0 :: Int, 1) gen
      pure . Num . fromIntegral $ arg
    2 -> do
      idx <- uniformRM (0 :: Int, length ps - 1) gen
      pure . Param . (!! idx) $ ps
    3 -> do
      r1 <- gf gen
      r2 <- gf gen
      pure $ Add r1 r2
    4 -> do
      r1 <- gf gen
      r2 <- gf gen
      pure $ Mul r1 r2
    _ -> error "Unreachable!"
 where
  gf = generateFunction ps

computeFunction :: Map Text Double -> Expr -> Double
computeFunction m e = case e of
  Num x -> x
  Param n -> m ! n
  Add e1 e2 -> cf e1 + cf e2
  Mul e1 e2 -> cf e1 * cf e2
 where
  cf = computeFunction m

main :: IO ()
main = do
  gen <- newStdGen >>= newIOGenM
  fun <- generateFunction ["x", "y"] gen
  print fun
  let size = 1000 :: Int
  let values =
        map
          ( \n ->
              let (x, y) = n `divMod` size
                  v = computeFunction [("x", fromIntegral x / fromIntegral size), ("y", fromIntegral y / fromIntegral size)] fun
               in (v, n)
          )
          ([0 .. size * size - 1] :: [Int])
  let maxValue = fst $ maximumBy (compare `on` fst) values
  print maxValue
  withFile "image.pgm" WriteMode $ \h -> do
    hPutStrLn h "P2"
    hPutStrLn h $ show size <> " " <> show size
    hPutStrLn h "255"
    mapM_
      ( \(v, n) ->
          let c = truncate $ v / maxValue * 255
           in do
                hPutStr h $ show @Int c <> " "
                when (n `mod` size == size - 1) $ hPutStrLn h "\n"
      )
      values
