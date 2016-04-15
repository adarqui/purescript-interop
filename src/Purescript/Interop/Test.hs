{-# LANGUAGE TemplateHaskell #-}

module Purescript.Interop.Test where

import Purescript.Interop



newtype Session = Session { unSession :: String }

mkExports defaultOptions Nothing [ (''Session, True) ] 0
mkExports defaultOptions (Just ("header", "footer", "/tmp/interop1.purs")) [ (''Session, True) ] 1
mkExports defaultOptionsClean (Just ("header", "footer", "/tmp/interop1.clean.purs")) [ (''Session, True) ] 101



data SumType
  = A
  | B Int
  | C Bool
  | D String
  | E [Int]
  | F SumType
  | G [SumType]

mkExports defaultOptions (Just ("header", "footer", "/tmp/interop2.purs")) [ (''SumType, True) ] 2
mkExports defaultOptionsClean (Just ("header", "footer", "/tmp/interop2.clean.purs")) [ (''SumType, True) ] 102



data BigRecord = BigRecord {
  bigRecordBool :: Bool,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: Maybe Int,
  bigRecordInteger :: Integer,
  bigRecordMaybeInteger :: Maybe Integer,
  bigRecordString :: String,
  bigRecordSumType :: SumType
}

mkExports defaultOptions (Just ("header", "footer", "/tmp/interop3.purs")) [ (''BigRecord, True) ] 3
mkExports defaultOptionsClean (Just ("header", "footer", "/tmp/interop3.clean.purs")) [ (''BigRecord, True) ] 103



newtype DateMaybe = DateMaybe (Maybe String)

mkExports defaultOptions (Just ("header", "footer", "/tmp/interop4.purs")) [ (''DateMaybe, True) ] 4
mkExports defaultOptionsClean (Just ("header", "footer", "/tmp/interop4.clean.purs")) [ (''DateMaybe, True) ] 104
