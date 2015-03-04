{-# LANGUAGE TemplateHaskell #-}
module LambdaQuoter where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Lambda

import Control.Category
import Control.Category.Cartesian
import Control.Category.Cartesian.Closed
import Control.Categorical.Object

import qualified Data.Map as M

import Prelude hiding (curry,fst,snd)

lambda :: QuasiQuoter
lambda = QuasiQuoter quoteLambdaExp quoteLambdaPat quoteLambdaType quoteLambdaDec

quoteLambdaPat = error "Quoting lambdas in patterns are not supported"

quoteLambdaType = error "Quoting lambdas in types are not supported"

quoteLambdaDec = error "Quoting declarations of lambdas is not supported"

quoteLambdaExp :: String -> TH.ExpQ
quoteLambdaExp s = 
    do loc <- TH.location
       let pos = (TH.loc_filename loc,
                  fst (TH.loc_start loc),
                  snd (TH.loc_start loc))
       expr <- parseExpr pos s
       lambdaToCat expr

type Env = (Int,M.Map String TH.ExpQ)

lambdaToCat :: Lambda () Int -> TH.ExpQ
lambdaToCat (Var 0) = [| snd |]
lambdaToCat (Var n) = [| fst >>> $(lambdaToCat (Var (n-1))) |]
lambdaToCat (App e1 e2)
    = [| $(lambdaToCat e1) &&& $(lambdaToCat e1) >>> apply |]
lambdaToCat (Lam v e)
    = [| curry $(lambdaToCat e) |]
lambdaToCat (Fst e)
    = [| $(lambdaToCat e) >>> fst |]
lambdaToCat (Snd e)
    = [| $(lambdaToCat e) >>> snd |]
lambdaToCat (Pair (e1,e2))
    = [| $(lambdaToCat e1) &&& $(lambdaToCat e2) |]
lambdaToCat (Unit)
    = [| terminate |]
{-
lambdaToCat (Inl e)
    = [| $(lambdaToCat e) >>> inl |]
lambdaToCat (Inr e)
    = [| $(lambdaToCat e) >>> inr |]
lambdaToCat (Case e _ e1 _ e2)
    = [| $(lambdaToCat e) >>> ($(lambdaToCat e1) 
                               ||| $(lambdaToCat e2)) |]
-}
lambdaToCat (AntiQ v)
    = TH.varE (TH.mkName v)
