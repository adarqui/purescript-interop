{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -ddump-splices #-}

module Purescript.Interop where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Exception
import Control.Monad

import Data.Monoid
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import qualified Data.ByteString.Lazy as B
import Data.Hashable
import Data.List
import Data.Maybe

import Debug.Trace

import Data.Transform.UnCamel

instance Lift Type where
  lift (ConT n) = [| ConT (mkName nstr) |] where nstr = show n
  lift (AppT a b) = [| AppT a b |]
  lift (TupleT x) = [| TupleT x |]
  lift (ListT) = [| ListT |]

--------------------------------------------------------------------------------

data Codec
  = CodecJSON
  | CodecArgonaut
  | CodecNone
  deriving (Show)

type StringTransformFn = String -> String -> String

data InteropOptions = InteropOptions {
  fieldNameTransform :: StringTransformFn,
  jsonNameTransform :: StringTransformFn,
  jsonTagNameTransform :: StringTransformFn,
  createLenses :: Bool,
  codec :: Codec,
  indent :: Int
}

defaultOptions :: InteropOptions
defaultOptions = InteropOptions {
  fieldNameTransform = defaultFieldNameTransform,
  jsonNameTransform = defaultJsonNameTransform,
  jsonTagNameTransform = defaultJsonTagNameTransform,
  createLenses = False,
  codec = CodecJSON,
  indent = 2
}

defaultFieldNameTransform :: StringTransformFn
defaultFieldNameTransform nb s = s

defaultJsonNameTransform :: StringTransformFn
defaultJsonNameTransform nb s = s

defaultJsonTagNameTransform :: StringTransformFn
defaultJsonTagNameTransform nb s = s



defaultOptionsClean :: InteropOptions
defaultOptionsClean = InteropOptions {
  fieldNameTransform = defaultFieldNameTransformClean,
  jsonNameTransform = defaultJsonNameTransformClean,
  jsonTagNameTransform = defaultJsonTagNameTransformClean,
  createLenses = True,
  codec = CodecJSON,
  indent = 2
}

defaultFieldNameTransformClean :: StringTransformFn
defaultFieldNameTransformClean nb s =
  if isPrefixOf ftl s
    then firstToLower $ fromJust $ stripPrefix ftl s
    else s
  where
  ftl = firstToLower nb

defaultJsonNameTransformClean :: StringTransformFn
defaultJsonNameTransformClean nb s =
  if isPrefixOf ftl s
    then map toLower $ unCamelSource '_' $ fromJust $ stripPrefix ftl s
    else s
  where
  ftl = firstToLower nb


defaultJsonTagNameTransformClean :: StringTransformFn
defaultJsonTagNameTransformClean nb s = s


--------------------------------------------------------------------------------

data InternalRep
-- TODO FIXME: turn Normal's into String (Maybe String), for empty constructors
--
  = NewtypeRecIR String [(String, String)]
  | NewtypeNormalIR String String
  | DataIR String [InternalRep]
  | DataRecIR String [(String, String)]
  | DataNormalIR String String
  | TypeIR String String
  | EmptyIR
  deriving (Show)

--------------------------------------------------------------------------------

-- name, args, hash
type API = [(String, [Type], String)]


type SerAPI = [(String, String)]

class Export a where
  export :: a -> String

emptyApi :: SerAPI
emptyApi = []

parseCall :: Value -> Parser (String, String, Value)
parseCall (Object o) = do
  call    <- o .: "call"
  version <- o .: "version"
  args    <- o .: "args"
  return (call, version, args)
parseCall _ = fail "Could not parse RPC call"



firstToLower :: String -> String
firstToLower [] = []
firstToLower (x:xs) = toLower x:xs



-- rev is temporary
--
mkExports :: Int -> InteropOptions -> Maybe (String, String, FilePath) -> [(Name, Bool)] -> Q [Dec]
mkExports rev InteropOptions{..} out ts = do
  exports <- forM ts $ \(t, json) -> do
    TyConI dec <- reify t
    let ir = parseInternalRep dec
    return $
      mkExport dec
      ++ show ir
      ++ "\n\n" ++ mkBuilder dec ++ "\n\n"
      ++ (if createLenses
           then "\n\n" ++ mkLenses dec ++ "\n\n"
           else "")
      ++ (if json
           then "\n\n" ++ mkToJson dec ++ "\n\n" ++ mkFromJson dec
           else "")

  let exports' = commonPurescriptImports ++ intercalate "\n\n" exports
      handleAll :: SomeException -> IO ()
      handleAll _ = return ()

  exportsDec <- valD (varP $ mkName $ "rpcExports_" <> show rev)
                     (normalB $ litE $ stringL exports')
                     []

  case out of
    Just (header, footer, path) -> runIO $ handle handleAll $ writeFile path (header ++ "\n\n" ++ exports' ++ "\n\n" ++ footer)
    Nothing -> return ()

  return [exportsDec]

    where

      parseInternalRep (NewtypeD _ n tyvars con _) = mkConNewtypeIR (nameBase n) con
      parseInternalRep (DataD _ n tyvars cons _) = DataIR (nameBase n) $ map (mkConDataIR (nameBase n)) cons
      parseInternalRep (TySynD n tyvars t) = TypeIR (nameBase n) (mkTypeIR t)
      parseInternalRep _ = EmptyIR

      mkConNewtypeIR nb (RecC n vars) = NewtypeRecIR nb (map (mkVarIR nb) vars)
      mkConNewtypeIR nb (NormalC n vars) = NewtypeNormalIR (nameBase n) (intercalate " " (map mkVarIR' vars))
      mkConNewtypeIR nb _ = EmptyIR

      mkConDataIR nb (RecC n vars) = DataRecIR nb (map (mkVarIR nb) vars)
      mkConDataIR nb (NormalC n vars) = DataNormalIR (nameBase n) (intercalate " " (map mkVarIR' vars))
      mkConDataIR nb _ = EmptyIR

      mkVarIR nb (n, _, t) = (fieldNameTransform nb (nameBase n), mkTypeIR t)
      mkVarIR' (_, t) = mkTypeIR t

      mkTypeIR (ConT n) | nameBase n == "Set" = "Array"
                      | nameBase n == "Bool" = "Boolean"
      mkTypeIR (ConT n) = nameBase n
      mkTypeIR (VarT a) = takeWhile (/= '_') $ nameBase a
      mkTypeIR (AppT f x) = "(" ++ mkType f ++ " " ++ mkTypeIR x ++ ")"
      mkTypeIR (TupleT 0) = "Unit "
      mkTypeIR (TupleT 2) = "Tuple "
      mkTypeIR (TupleT n) = "Tuple" ++ show n ++ " "
      mkTypeIR ListT = "Array "

      mkTyVarIR (PlainTV n) = nameBase n
      mkTyVarIR (KindedTV n _) = nameBase n

      mkLenses (NewtypeD _ n tyvars con _)
        = "_" ++ nameBase n ++ " :: LensP " ++ nameBase n ++ " "
        ++ mkCon' (nameBase n) con
        ++ "\n"
        ++ "_" ++ nameBase n ++ " f (" ++ nameBase n ++ " o) = " ++ nameBase n ++ " <$> f o"
      mkLenses _ = ""

      mkBuilder (NewtypeD _ n tyvars con _)
        =  "mk" ++ nameBase n ++ " :: " ++ "\n"
        ++ "mk" ++ nameBase n ++ " .. " ++ "\n"
        ++ "  { ... }"
      mkBuilder _ = ""

      mkExport (NewtypeD _ n tyvars con _)
        = "newtype " ++ nameBase n ++ " " ++ intercalate " " (map mkTyVar tyvars) ++ " = "
        ++ mkCon (nameBase n) con
      mkExport (DataD _ n tyvars cons _)
        = "data " ++ nameBase n ++ " " ++ intercalate " " (map mkTyVar tyvars) ++ " = "
        ++ intercalate "  | " (map (mkCon (nameBase n)) cons)
      mkExport (TySynD n tyvars t)
        = "type " ++ nameBase n ++ " " ++ intercalate " " (map mkTyVar tyvars) ++ " = "
        ++ mkType t

      mkCon nb (RecC n vars) = nameBase n ++ " {\n" ++ intercalate ",\n" (map (mkVar nb) vars) ++ "\n}"
      mkCon nb (NormalC n vars) = nameBase n ++ " " ++ intercalate " " (map mkVar' vars) ++ "\n"

      mkCon' nb (RecC n vars) = "{\n" ++ intercalate ",\n" (map (mkVar nb) vars) ++ "\n}"
      mkCon' nb _ = "{ empty :: Boolean }"

      mkVar nb (n, _, t) = "  " ++ fieldNameTransform nb (nameBase n) ++ " :: " ++ mkType t
      mkVar' (_, t) = mkType t

      mkType (ConT n) | nameBase n == "Set" = "Array"
                      | nameBase n == "Bool" = "Boolean"
      mkType (ConT n) = nameBase n
      mkType (VarT a) = takeWhile (/= '_') $ nameBase a
      mkType (AppT f x) = "(" ++ mkType f ++ " " ++ mkType x ++ ")"
      mkType (TupleT 0) = "Unit "
      mkType (TupleT 2) = "Tuple "
      mkType (TupleT n) = "Tuple" ++ show n ++ " "
      mkType ListT = "Array "

      mkTyVar (PlainTV n) = nameBase n
      mkTyVar (KindedTV n _) = nameBase n

      -- ToJSON deriving

      -- TODO: add ToJSON constraints
      mkToJson (NewtypeD _ n tyvars con _) = concat
        [ "instance " ++ firstToLower (nameBase n) ++ "ToJson :: "
           ++ mkConstraints "ToJSON" tyvars
           ++ " ToJSON (" ++ nameBase n ++ " "
           ++ intercalate " " (map mkTyVar tyvars) ++ ") where\n"
        , case con of
            NormalC _ _ -> "  toJSON (" ++ conToName con ++ " x) = toJSON x"
            RecC _ _ -> mkConToJson (nameBase n) con
        ]
      mkToJson (DataD _ n tyvars cons _) = concat
        [ "instance " ++ firstToLower (nameBase n) ++ "ToJson :: "
           ++ mkConstraints "ToJSON" tyvars
           ++ " ToJSON (" ++ nameBase n ++ " "
           ++ intercalate " " (map mkTyVar tyvars) ++ ") where\n"
        , concatMap (mkConToJson (nameBase n)) cons
        ]
      mkToJson (TySynD n tyvars t) = ""

      conToName (RecC n _) = nameBase n
      conToName (NormalC n _) = nameBase n

      mkConToJson nb (RecC n vars) = concat
        [ "  toJSON (" ++ nameBase n ++ " v) = object $\n"
        , "    [ \"tag\" .= \"" ++ jsonTagNameTransform nb (nameBase n) ++ "\"\n"
        , concatMap (mkVarToJson nb) vars
        , "    ]\n"
        ]
      mkConToJson nb (NormalC n vars) = concat
        [ "  toJSON (" ++ nameBase n ++ " " ++ intercalate " " vars' ++ ") = object $\n"
        , "    [ \"tag\" .= \"" ++ jsonTagNameTransform nb (nameBase n) ++ "\"\n"
        , if null vars
            then "    , \"contents\" .= ([] :: Array String)\n"
            else "    , \"contents\" .= " ++ wrapContent vars (intercalate ", " (map ("toJSON " ++) vars')) ++ "\n"
        , "    ]\n"
        ]
        where vars' = map (("x" ++) . show) [0..length vars - 1]

      wrapContent :: [a] -> String -> String
      wrapContent vars str | length vars == 1 = str
                           | otherwise        = "[" ++ str ++ "]"

      mkVarToJson nb (n, _, _) = "    , \"" ++ jsonNameTransform nb (nameBase n) ++ "\" .= v." ++ (fieldNameTransform nb (nameBase n)) ++ "\n"

      -- FromJSON deriving

      mkConstraints _ [] = ""
      mkConstraints cns tyvars = "(" ++ intercalate ", " (map (\tv -> cns ++ " " ++ mkTyVar tv) tyvars) ++ ") => "

      mkFromJson (NewtypeD _ n tyvars con _) = concat
        [ "instance " ++ firstToLower (nameBase n) ++ "FromJson :: "
           ++ mkConstraints "FromJSON" tyvars
           ++ " FromJSON (" ++ nameBase n ++ " "
           ++ intercalate " " (map mkTyVar tyvars)
           ++ ") where\n"
        , case con of
            NormalC _ _ -> "  parseJSON x = " ++ conToName con ++ " <$> parseJSON x"
            RecC _ _ -> "  parseJSON (JObject o) = do\n" ++ mkConFromJson (nameBase n) False con ++ "\n"
                     ++ "  parseJSON x = fail $ \"Could not parse object: \" ++ show x"
        ]
      mkFromJson (DataD _ n tyvars cons _) = concat
        [ "instance " ++ firstToLower (nameBase n) ++ "FromJson :: "
           ++ mkConstraints "FromJSON" tyvars
           ++ " FromJSON (" ++ nameBase n ++ " "
           ++ intercalate " " (map mkTyVar tyvars)
           ++ ") where\n"
        , if length cons == 1 && (isNullaryCon $ head cons)
            then "  parseJSON _ = do\n"
            else concat
              [ "  parseJSON (JObject o) = do\n"
              , if length cons > 1
                  then concat
                    [ "    tag <- o .: \"tag\"\n"
                    , "    case tag of\n"
                    ]
                  else ""
              ]
        , concatMap (mkConFromJson (nameBase n) (length cons > 1)) cons
        , if length cons == 1 && (isNullaryCon $ head cons)
            then ""
            else "  parseJSON x = fail $ \"Could not parse object: \" ++ show x"
        ]
      mkFromJson (TySynD n tyvars t) = ""

      isNullaryCon (RecC n []) = True
      isNullaryCon (NormalC n []) = True
      isNullaryCon _ = False

      mkConFromJson nb useCase (RecC n vars) = concat
        [ if useCase then "      \"" ++ nameBase n ++ "\" -> do\n" else ""
        , concatMap (mkVarFromJson nb) vars
        , "        return $ " ++ nameBase n ++ " { "
        , intercalate ", " $ map (\(n, _, _) -> fieldNameTransform nb (nameBase n) ++ " : " ++ fieldNameTransform nb (nameBase n)) vars
        , " }\n"
        ]
      mkConFromJson nb useCase (NormalC n vars) = concat
        [ if useCase then "      \"" ++ nameBase n ++ "\" -> do\n" else ""
        , if null vars'
            then "         return " ++ nameBase n ++ "\n"
            else concat
              [ "         " ++ wrapContent vars (intercalate ", " vars') ++  " <- o .: \"contents\"\n"
              , "         " ++ nameBase n ++ " <$> " ++ intercalate " <*> " (map ("parseJSON " ++) vars') ++ "\n"
              ]
        , "\n"
        ]
        where vars' = map (("x" ++) . show) [0..length vars - 1]

      mkVarFromJson nb (n, _, _) = "        " ++ fieldNameTransform nb (nameBase n) ++ " <- o .: \"" ++ jsonNameTransform nb (nameBase n) ++ "\"\n"

mkRPC :: Bool -> [Name] -> Q [Dec]
mkRPC checkver fs = do
  rpc <- funD (mkName "handleRPC")
    [clause
      []
      ( normalB $ lamE [varP json]
        $ caseE [| parse parseCall $(varE json) |]
                $ (map mkMatch fs)
               ++ [ match [p| _ |]
                          (normalB [| Error "No such endpoint" |])
                          []
                  ]
      )
      []
    ]

  api <- forM fs $ \f -> do
    (cons, args) <- gatherArgTypes f
    return (nameBase f, cons, args)

  defs <- valD (varP $ mkName "rpcDefs")
               (normalB [e| api |])
               []

  -- check API version
  when checkver $ do
    file <- runIO $ try $ B.readFile "api.json" :: Q (Either SomeException B.ByteString)

    let oldApi = either (const emptyApi) (fromMaybe emptyApi . decode) file
        serApi = map (\(a, _, c) -> (a, c)) api
        diff   = oldApi \\ serApi

    when (not $ null diff) $
      error $ "Versions lost in new API: " ++ show diff
           ++ " -- either delete line(s) from api.json or add version(s) back."

    runIO $ B.writeFile "api.json" $ encode serApi

  return [rpc, defs]

  where
    json = mkName "json"
    v = mkName "v"

    gatherCons (AppT (AppT ArrowT app) x) = app:gatherCons x
    gatherCons x@(AppT _ _) = [x]
    gatherCons x@(ConT _) = [x]
    gatherCons (ForallT _ _ x) = gatherCons x
    gatherCons _ = []

    -- TODO: source out
    gatherArgTypes f = do
      VarI _ apps _ _ <- reify f
      let cons = gatherCons apps

      -- TODO: make fingerprinting stable
      let rfcon [ConT n] = [reify n]
          rfcon [VarT n] = [return $ TyConI $ DataD [] n [] [] []]
          rfcon [AppT f x] = rfcon [f] ++ rfcon [x]
          rfcon [TupleT x] = [return $ TyConI $ DataD [] (mkName $ "tuple" ++ show x) [] [] []]
          rfcon [ListT] = [return $ TyConI $ DataD [] (mkName $ "list") [] [] []]
          rfcon (x:xs) = rfcon [x] ++ rfcon xs
          rfcon [] = []

          -- remove kind variables, beacause those are not stable (exact names
          -- are random) and thus not suitable for hashing
          unkind (TyConI (DataD a b _ d e)) = TyConI (DataD a b [] d e)
          unkind (TyConI (NewtypeD a b _ d e)) = TyConI (NewtypeD a b [] d e)
          unkind (TyConI (TySynD a _ c)) = TyConI (TySynD a [] c)
          unkind x = x

      args <- sequence $ rfcon cons
      return (cons, show $ hash $ show $ map unkind args)

    mkMatch f = do
      (cons, args) <- gatherArgTypes f
      match [p| Success ( $(litP $ stringL $ nameBase f)
                        , $(litP $ stringL args)
                        , $(varP v)
                        )
            |]
            (normalB [| fmap toJSON <$> ($(mkPat f cons) <$> fromJSON $(varE v)) |])
            []

    mkPat f cons = do
      args <- mapM (newName . ("x" ++ ) . show) [0..length cons - 2]
      lamE [vargs args] (foldl appE (varE f) (map varE args))
      where vargs [] = error "Empty argument list"
            vargs [x] = varP x
            vargs (x:xs) = tupP [varP x, vargs xs]


traceLog msg x = trace (msg ++ show x) x

genPurescriptRpcs :: API -> String
genPurescriptRpcs = concatMap gen
  where
    gen :: (String, [Type], String) -> String
    gen (name, sig, hash) = concat
      [ name ++ " :: forall eff. Socket.Socket -> " ++ genSig sig ++ "\n"
      , name ++ " socket " ++ intercalate " " args ++ "\n"
      , if isAsync (last sig)
        then " = send socket $ encode \n"
        else " = sendSync socket $ encode \n"
      , "   $ object [ \"call\" .= \"" ++ name ++ "\"\n"
      , "            , \"version\" .= \"" ++ hash ++ "\"\n"
      , "            , \"args\" .= " ++ vargs args ++ "\n"
      , "            ]\n"
      , "\n"
      ]
      where args = map (("x" ++) .show) [0..length sig - 2]
            vargs :: [String] -> String
            vargs [] = ""
            vargs [x] = x
            vargs (x:xs) = "(Tuple " ++ x ++ " " ++ vargs xs ++ ")"

    isAsync (AppT _ (TupleT 0)) = True
    isAsync (TupleT 0) = True
    isAsync _ = False

    genSig [ConT n] | nameBase n == "Set" = "Array"
                    | nameBase n == "Bool" = "Boolean"
    genSig [ConT n] = nameBase n
    genSig [AppT f x] = "(" ++ genSig [f] ++ " " ++ genSig [x] ++ ")"
    genSig [ListT] = "Array"
    genSig [TupleT 0] = "Unit"
    genSig [TupleT 2] = "Tuple"
    genSig [TupleT n] = "Tuple" ++ show n ++ " "
    genSig [x] = error $ "Not supported type in function signature: " ++ show x
    genSig [x, (AppT (ConT n) r)] | nameBase n == "IO" = genSig [x] ++ " -> Aff (websocket :: Socket.WebSocket | eff) " ++ genSig [r]
    genSig [x, r] = genSig [x] ++ " -> Aff (websocket :: Socket.WebSocket | eff) " ++ genSig [r]
    genSig (x:xs) = genSig [x] ++ " -> " ++ genSig xs
    genSig [] = ""

commonPurescriptImports :: String
commonPurescriptImports = intercalate "\n"
  [ ""
  , ""
  , "import Data.JSON"
  , "import Data.Either"
  , "import Data.Maybe"
  , "import Data.List (List ())"
  , "import Data.Tuple"
  , "import Data.Set (Set ())"
  , "import Optic.Lens"
  , "import Optic.Core"
  , "import Control.Monad.Aff"
  , "import Prelude"
  , ""
  , "type Text = String"
  , ""
  , ""
  ]

{- This all is needed for Type to be a JSON instance

deriveJSON defaultOptions ''ModName
deriveJSON defaultOptions ''PkgName
deriveJSON defaultOptions ''NameSpace
deriveJSON defaultOptions ''NameFlavour
deriveJSON defaultOptions ''TyLit
deriveJSON defaultOptions ''OccName
deriveJSON defaultOptions ''TyVarBndr
deriveJSON defaultOptions ''Name
deriveJSON defaultOptions ''Type

-}

