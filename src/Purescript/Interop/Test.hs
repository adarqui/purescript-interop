{-# LANGUAGE TemplateHaskell #-}

module Purescript.Interop.Test where

import Purescript.Interop

newtype Session = Session { unSession :: String }

data SumType
  = A
  | B Int
  | C Bool
  | D String
  | E [Int]
  | F SumType
  | G [SumType]

data BigRecord = BigRecord {
  bigRecordBool :: Bool,
  bigRecordInt :: Int,
  bigRecordMaybeInt :: Maybe Int,
  bigRecordInteger :: Integer,
  bigRecordMaybeInteger :: Maybe Integer,
  bigRecordString :: String,
  bigRecordSumType :: SumType
}

newtype DateMaybe = DateMaybe (Maybe String)

type Text = String

type TextMaybe = Maybe Text

data FunkyRecord
  = Boom1 { boom1 :: Bool }
  | Boom2 { boom2 :: Bool }
  | Boom3 { boom3a :: Bool, boom3b :: Bool, boom3c :: Bool }
  | Boom4


mkExports 1 defaultOptions (Just ("header", "footer", "/tmp/interop1.purs"))
  [
    (''Session, True),
    (''SumType, True),
    (''BigRecord, True),
    (''DateMaybe, True),
    (''Text, False),
    (''TextMaybe, False),
    (''FunkyRecord, True)
  ]



mkExports 2 defaultOptionsClean (Just ("header", "footer", "/tmp/interop1.clean.purs"))
  [
    (''Session, True),
    (''SumType, True),
    (''BigRecord, True),
    (''DateMaybe, True),
    (''Text, False),
    (''TextMaybe, False),
    (''FunkyRecord, True)
  ]
