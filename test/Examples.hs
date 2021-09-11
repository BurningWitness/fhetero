module Main where

import           Examples.FHDecision

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Prelude



main :: IO ()
main =
  void . evaluate . force $ refDecision Nothing
