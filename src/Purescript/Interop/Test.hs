{-# LANGUAGE TemplateHaskell #-}

module Purescript.Interop.Test where

import Purescript.Interop



newtype Session = Session { unSession :: String }

mkExports defaultOptions Nothing [ (''Session, True) ] 0
mkExports defaultOptions (Just ("header", "footer", "/tmp/interop1.purs")) [ (''Session, True) ] 1



data SumType
  = A
  | B Int
  | C Bool
  | D String
  | E [Int]
  | F SumType
  | G [SumType]

mkExports defaultOptions (Just ("header", "footer", "/tmp/interop2.purs")) [ (''SumType, True) ] 2



data BigRecord = BigRecord {
  fBool :: Bool,
  fInt :: Int,
  fMaybeInt :: Maybe Int,
  fInteger :: Integer,
  fMaybeInteger :: Maybe Integer,
  fString :: String,
  fSumType :: SumType
}

mkExports defaultOptions (Just ("header", "footer", "/tmp/interop3.purs")) [ (''BigRecord, True) ] 3
