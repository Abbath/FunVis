{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Picture qualified as J
import Control.Monad (forM_, when)
import Data.ByteString qualified as BS
import Data.Foldable (maximumBy, minimumBy)
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector.Storable qualified as V
import Data.Word
import ExprParser (Cond (..), Expr (..), parseExpr)
import Options.Applicative
import System.FilePath (replaceBaseName, takeBaseName, (-<.>))
import System.IO (IOMode (WriteMode), hPutStrLn, hSetBinaryMode, withFile)
import System.Random.Stateful
import Text.Megaparsec.Error
import Text.Printf

-- data Expr = Param Text | Num Double | Add Expr Expr | Mul Expr Expr | Pow Expr | Fun Text Expr deriving (Show)

showT :: (Show a) => a -> Text
showT = pack . show

prettyPrint :: Expr -> Text
prettyPrint (Param p) = p
prettyPrint (Num x) = showT x
prettyPrint (Add e1 e2) = "(" <> prettyPrint e1 <> " + " <> prettyPrint e2 <> ")"
prettyPrint (Mul (Num (-1)) e2) = "(-" <> prettyPrint e2 <> ")"
prettyPrint (Mul e1 e2) = "(" <> prettyPrint e1 <> " * " <> prettyPrint e2 <> ")"
prettyPrint (Pow n e1) = "(" <> prettyPrint e1 <> "^" <> showT n <> ")"
prettyPrint (Fun f e1) = f <> "(" <> prettyPrint e1 <> ")"
prettyPrint (If cond a b c d) = "if(" <> prettyPrint a <> (if cond == Equal then "==" else "<") <> prettyPrint b <> ", " <> prettyPrint c <> ", " <> prettyPrint d <> ")"

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
        | choice < w5 ws -> Pow <$> randomChoice [2.0, 3.0] <*> gf
        | choice < w6 ws -> If <$> randomChoice [Equal, Less] <*> gf <*> gf <*> gf <*> gf
        | otherwise -> Fun <$> randomChoice ["sin", "abs", "sqrt", "log", "inv"] <*> gf
 where
  gf = generateFunction (depth - 1) mc ps ws gen
  randomNumber = uniformRM (-mc, mc) gen
  randomChoice v = do
    idx <- uniformRM (0 :: Int, length v - 1) gen
    pure $ v !! idx

computeFunction :: Map Text Double -> Expr -> Double
computeFunction m e = case e of
  Num x -> x
  Param n -> m ! n
  Add e1 e2 -> cf e1 + cf e2
  Mul e1 e2 -> cf e1 * cf e2
  Pow n e1 -> cf e1 ** n
  Fun "sin" e1 -> sin $ cf e1
  Fun "abs" e1 -> abs $ cf e1
  Fun "sqrt" e1 -> sqrt $ cf e1
  Fun "log" e1 -> log $ cf e1
  Fun "inv" e1 -> let d = cf e1 in if d == 0 then d else 1 / cf e1
  If cond a b c d ->
    if case cond of
      Equal -> cf a == cf b
      Less -> cf a < cf b
      then cf c
      else cf d
  _ -> error "Unreachable!"
 where
  cf = computeFunction m

data Rgb = Rgb {r :: Double, g :: Double, b :: Double}

data Output = PngOutput | TextOutput | BinaryOutput deriving (Eq, Show)

parseOutput :: ReadM Output
parseOutput = eitherReader $ \case
  "png" -> pure PngOutput
  "text" -> pure TextOutput
  "bin" -> pure BinaryOutput
  _ -> error "Wrong output format"

data Options = Options
  { maxDepth :: Int
  , fieldSize :: Double
  , imageWidth :: Int
  , imageHeight :: Int
  , maxConstant :: Double
  , weights :: Text
  , outputType :: Output
  , output :: FilePath
  , funR :: Text
  , funG :: Text
  , funB :: Text
  , attempts :: Int
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
    <*> option parseOutput (long "output-format" <> short 'f' <> help "Output format (png, text, bin)" <> value PngOutput <> metavar "FORMAT")
    <*> strOption (long "output" <> short 'o' <> help "Output file name" <> showDefault <> value "image.png" <> metavar "OUTPUT")
    <*> strOption (long "red" <> short 'r' <> help "Red channel function" <> value "" <> metavar "RED_FUNCTION")
    <*> strOption (long "green" <> short 'g' <> help "Green channel function" <> value "" <> metavar "GREEN_FUNCTION")
    <*> strOption (long "blue" <> short 'b' <> help "Blue channel function" <> value "" <> metavar "BLUE_FUNCTION")
    <*> option auto (long "attempts" <> short 'a' <> help "Number of attempts" <> showDefault <> value 0 <> metavar "ATTEMPTS")

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
    n -> forM_ ([1 .. n] :: [Int]) $ perform args
 where
  opts = info (options <**> helper) (fullDesc <> progDesc "Function Visualisation" <> header "Visualise a random function")

perform :: Options -> Int -> IO ()
perform args idx = do
  gen_r <- newStdGen >>= newIOGenM
  gen_g <- newStdGen >>= newIOGenM
  gen_b <- newStdGen >>= newIOGenM
  let ws = read @[Double] . T.unpack . ("[" <>) . (<> "]") . T.intercalate ", " . T.words $ weights args
  let normWeights = normalizeWieghts (Weights (head ws) (ws !! 1) (ws !! 2) (ws !! 3) (ws !! 4) (ws !! 5) (ws !! 6))
  let gf = generateFunction (maxDepth args) (maxConstant args) ["x", "y"] normWeights
  fun_r <-
    if funR args == ""
      then gf gen_r
      else pure . parseFunction $ funR args
  T.putStrLn $ prettyPrint fun_r
  fun_g <-
    if funG args == ""
      then gf gen_g
      else pure . parseFunction $ funG args
  T.putStrLn $ prettyPrint fun_g
  fun_b <-
    if funB args == ""
      then gf gen_b
      else pure . parseFunction $ funB args
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
               in Rgb (cfp fun_r) (cfp fun_g) (cfp fun_b)
          )
          ([0 .. width * height - 1] :: [Int])
  let (maxValue_r, minValue_r) = computeBounds r values
  let valueSpan_r = maxValue_r - minValue_r
  let (maxValue_g, minValue_g) = computeBounds g values
  let valueSpan_g = maxValue_g - minValue_g
  let (maxValue_b, minValue_b) = computeBounds b values
  let valueSpan_b = maxValue_b - minValue_b
  let values3 = V.concat $ map (\(Rgb r g b) -> V.fromList [compute r minValue_r valueSpan_r, compute g minValue_g valueSpan_g, compute b minValue_b valueSpan_b]) values
  let filename =
        if idx == -1
          then output args
          else replaceBaseName (output args) (takeBaseName (output args) <> "_" <> printf "%03d" idx)
  case outputType args of
    PngOutput -> J.writePng @J.PixelRGB8 (filename -<.> "png") (J.Image width height values3)
    _ -> withFile (filename -<.> "ppm") WriteMode $ \h -> do
      hPutStrLn h (if outputType args /= BinaryOutput then "P3" else "P6")
      hPutStrLn h $ show width <> " " <> show height
      hPutStrLn h "255 "
      when (outputType args == BinaryOutput) $ hSetBinaryMode h True
      mapM_
        ( \(Rgb v_r v_g v_b) ->
            let c_r = compute v_r minValue_r valueSpan_r
                c_g = compute v_g minValue_g valueSpan_g
                c_b = compute v_b minValue_b valueSpan_b
             in case outputType args of
                  BinaryOutput -> BS.hPut h [fromIntegral c_r, fromIntegral c_g, fromIntegral c_b]
                  TextOutput -> hPutStrLn h $ show @Word8 c_r <> " " <> show @Word8 c_g <> " " <> show @Word8 c_b
        )
        values
 where
  compute :: Double -> Double -> Double -> Word8
  compute v minValue valueSpan = fromInteger . (`mod` 256) . truncate $ (v - minValue) / valueSpan * 255
  computeBound f g values = f $ g (compare `on` f) values
  computeBounds f values = (computeBound f maximumBy values, computeBound f minimumBy values)
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
