module Main (main) where

import Control.Monad (forM_)
import Dis
import Options.Applicative
import Performer
import Types

main :: IO ()
main = do
  args <- execParser opts
  if discord args
    then discordImage
    else do
      funs <- generateFunctions args
      case attempts args of
        0 -> perform funs args (-1)
        n ->
          forM_ [0 .. n - 1] $
            if step args /= 0
              then perform funs args
              else \m -> do
                new_funs <- generateFunctions args
                perform new_funs args m
 where
  opts = info (options <**> helper) (fullDesc <> progDesc "Function Visualisation" <> header "Visualise a random function")
