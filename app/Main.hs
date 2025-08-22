{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Text (Text)
import System.IO (IOMode (WriteMode), hPutStr, hPutStrLn, withFile)
import System.Random (StdGen, newStdGen, randomR)

data Expr = Param Text | Num Double | Add Expr Expr | Mul Expr Expr deriving (Show)

generateFunction :: [Text] -> StdGen -> Expr
generateFunction ps = fst . go ps
 where
  go p g =
    let (r, g1) = randomR (1 :: Int, 4) g
     in case r of
          1 -> let (arg, g2) = randomR (0 :: Int, 1) g1 in (Num $ fromIntegral arg, g2)
          2 -> let (idx, g2) = randomR (0 :: Int, length p - 1) g1 in (Param $ p !! idx, g2)
          3 ->
            let (r1, g2) = go p g1
                (r2, g3) = go p g2
             in (Add r1 r2, g3)
          4 ->
            let (r1, g2) = go p g1
                (r2, g3) = go p g2
             in (Mul r1 r2, g3)
          _ -> error "Unreachable!"

computeFunction :: Map Text Double -> Expr -> Double
computeFunction m e = case e of
  Num x -> x
  Param n -> m ! n
  Add e1 e2 -> computeFunction m e1 + computeFunction m e2
  Mul e1 e2 -> computeFunction m e1 * computeFunction m e2

main :: IO ()
main = do
  gen <- newStdGen
  let fun = generateFunction ["x", "y"] gen
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
  withFile "dupa.pgm" WriteMode $ \h -> do
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
