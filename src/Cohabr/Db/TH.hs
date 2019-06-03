{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Cohabr.Db.TH where

import Data.Generics.Uniplate.Data
import Data.List
import Data.Tuple.Select
import qualified Data.Profunctor.Product as PP
import qualified Data.Profunctor.Product.Default as D
import Data.Profunctor.Product.Default(Default)
import Language.Haskell.TH

makeTFAdaptorAndInstance :: String -> Name -> Q [Dec]
makeTFAdaptorAndInstance funName tyName = reify tyName >>= \case
  TyConI dec -> handleTyConDec funName dec
  _ -> fail "unsupported type"

handleTyConDec :: String -> Dec -> Q [Dec]
handleTyConDec funName (DataD _ tyName [tyVar] _ [con] _) = handleTyConCon funName tyName tyVarName con
  where tyVarName = case tyVar of PlainTV name -> name
                                  KindedTV name _ -> name
handleTyConDec _ _ = fail "Type shall have a single constructor and a single type variable"

handleTyConCon :: String -> Name -> Name -> Con -> Q [Dec]
handleTyConCon funName tyName tyVarName (RecC conName vars) =
  pure [makeInstanceDec funName tyName tyVarName conName $ sel3 <$> vars]
handleTyConCon _ _ _ _ = fail "Unsupported constructor type (only non-GADT records are supported)"

makeInstanceDec :: String -> Name -> Name -> Name -> [Type] -> Dec
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
    defBodyExp = (VarE $ mkName funName) `AppE` conEDefExp
    conEDefExp = foldl AppE (ConE conName) $ replicate (length vars) (VarE 'D.def)
    [p, a, b] = VarT . mkName <$> ["p", "a", "b"]
    replaceTyVar var = transformBi $ \case VarT n | n == tyVarName -> var
                                           ty -> ty
