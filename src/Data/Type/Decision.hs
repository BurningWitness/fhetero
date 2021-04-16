{-# LANGUAGE GADTs #-}

module Data.Type.Decision where

import           Data.Type.Map

import           Prelude



data Dec where
  Single :: Single -> Dec
  TChain :: [Dec] -> Dec

data Single where
  Action  :: a -> Single
  DChain  :: Dec -> Single
  DChoice :: M () -> Single
  TChoice :: Bool -> Dec -> Dec -> Single
