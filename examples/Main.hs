module Main where

import qualified Except
import qualified Reader

main = do
  putStrLn "Reader example:"
  Reader.main
  putStrLn "Except example:"
  Except.main
