{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Cohabr.Db.TH
( makeTFAdaptorAndInstance
, makeTable
, D.def
, module Opaleye
, module Opaleye.TypeFamilies
) where

import Data.Generics.Uniplate.Data
import Data.List
import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D
import Data.Profunctor.Product.Default(Default)
import Data.Tuple.Select
import Language.Haskell.TH
import qualified Opaleye.Map as M
import Opaleye
import Opaleye.TypeFamilies

makeTFAdaptorAndInstance :: String -> Name -> Q [Dec]
makeTFAdaptorAndInstance funName tyName = reify tyName >>= \case
  TyConI dec -> handleTyConDec funName dec
  _ -> fail "Unsupported type"

handleTyConDec :: String -> Dec -> Q [Dec]
handleTyConDec funName (DataD _ tyName [tyVar] _ [con] _) = handleTyConCon funName tyName tyVarName con
  where tyVarName = case tyVar of PlainTV name -> name
                                  KindedTV name _ -> name
handleTyConDec _ _ = fail "Type shall have a single constructor and a single type variable"

handleTyConCon :: String -> Name -> Name -> Con -> Q [Dec]
handleTyConCon funName tyName tyVarName (RecC conName vars) = pure [instDec, sigDec, funDec, tyInstDec]
  where
    funName' = mkName funName
    instDec = makeInstanceDec funName' tyName tyVarName conName $ sel3 <$> vars
    (sigDec, funDec) = makeFunDec funName' tyName conName vars
    tyInstDec = makeTyInstDec tyName
handleTyConCon _ _ _ _ = fail "Unsupported constructor type (only non-GADT records are supported)"

makeInstanceDec :: Name -> Name -> Name -> Name -> [Type] -> Dec
makeInstanceDec funName tyName tyVarName conName vars = InstanceD Nothing instCtx instTy [defDec]
  where
    instCtx = (ConT ''PP.ProductProfunctor `AppT` p) :
                [ ConT ''Default `AppT` p `AppT` replaceTyVar a ty `AppT` replaceTyVar b ty
                | ty <- nub vars
                ]
    instTy = ConT
                ''Default `AppT`
                p `AppT`
                (ConT tyName `AppT` a) `AppT`
                (ConT tyName `AppT` b)
    defDec = FunD (mkName "def") [Clause [] (NormalB defBodyExp) []]
    defBodyExp = VarE funName `AppE` conEDefExp
    conEDefExp = foldl AppE (ConE conName) $ replicate (length vars) (VarE 'D.def)
    [p, a, b] = VarT . mkName <$> ["p", "a", "b"]
    replaceTyVar var = transformBi $ \case VarT n | n == tyVarName -> var
                                           ty -> ty

makeFunDec :: Sel1 a Name => Name -> Name -> Name -> [a] -> (Dec, Dec)
makeFunDec funName tyName conName vars = (sigDec, funDec)
  where
    sigDec = SigD funName funTy
    funDec = FunD funName [Clause [VarP funPatVar] (NormalB funExp) []]
    [p, a, b] = VarT . mkName <$> ["p", "a", "b"]
    pab = InfixT (InfixT p ''(:<$>) a) ''(:<*>) b
    funTy = ForallT [] [ConT ''PP.ProductProfunctor `AppT` p] $
              ArrowT `AppT`
                (ConT tyName `AppT` pab) `AppT`
                (p `AppT`
                  (ConT tyName `AppT` a) `AppT`
                  (ConT tyName `AppT` b))
    funPatVar = mkName "row"
    ops = '(PP.***$) : repeat '(PP.****)
    args = [ VarE 'P.lmap `AppE` VarE varName `AppE` (VarE varName `AppE` VarE funPatVar)
           | varName <- sel1 <$> vars
           ]
    funExp = foldl (\l (op, r) -> InfixE (Just l) (VarE op) (Just r)) (ConE conName) $ zip ops args

makeTyInstDec :: Name -> Dec
makeTyInstDec tyName = TySynInstD ''M.Map $ TySynEqn [g, tyAppLhs] tyAppRhs
  where
    [g, f] = VarT . mkName <$> ["g", "f"]
    tyAppLhs = ConT tyName `AppT` (ConT ''F `AppT` f)
    tyAppRhs = ConT tyName `AppT` (ConT ''F `AppT` (ConT ''IMap `AppT` g `AppT` f))

makeTable :: Name -> Name -> String -> [String] -> Q [Dec]
makeTable tyName pFunName tableName fieldNames = reify tyName >>= \case
  TyConI (DataD _ _ _ _ [RecC conName vars] _)
    | length vars == length fieldNames -> pure $ makeTableDec tyName pFunName tableName fieldNames conName
    | otherwise -> fail "Fields count mismatch"
  _ -> fail "Unsupported type. The type shall be a type constructor with a single (non-GADT, record) constructor"

makeTableDec :: Name -> Name -> String -> [String] -> Name -> [Dec]
makeTableDec tyName pFunName tableName fieldNames conName =
  [ SigD funName funTy
  , FunD funName [Clause [] (NormalB funExp) []]
  ]
  where
    funName = mkName $ if "Table" `isSuffixOf` tableName
                          then tableName
                          else tableName <> "Table"
    tyCon = ConT tyName
    funTy = ConT ''Table `AppT` (tyCon `AppT` ConT ''W) `AppT` (tyCon `AppT` ConT ''O)
    funExp = VarE 'table `AppE`
              LitE (StringL tableName) `AppE`
              (VarE pFunName `AppE` tyVarExp)
    tyVarExp = foldl AppE (ConE conName)
                [ VarE 'tableField `AppE` LitE (StringL field)
                | field <- fieldNames
                ]
