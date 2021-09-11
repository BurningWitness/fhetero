{-# LANGUAGE FlexibleContexts
           , DataKinds
           , PartialTypeSignatures
           , RebindableSyntax #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Examples.FHDecision where

import           Data.FHDecision

import           Data.Proxy
import           Prelude hiding ((>>))



refDecision :: Maybe (Either Int Int) -> _
refDecision n = do
  action $ Just (2 :: Double)
  case n of
    Just (Left a)  -> (Proxy :: Proxy 0) >.. do action $ Just a
    Just (Right _) -> (Proxy :: Proxy 1) >.. do action $ Just ()
    Nothing        -> (Proxy :: Proxy 2) >!! do action $ Just False
  actions [Just (), Just (), Just ()]
  if Proxy :: Proxy 'True
    then action $ Just 'a'
    else action $ Just "a"
