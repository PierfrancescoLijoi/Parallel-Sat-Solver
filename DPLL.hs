{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DPLL (satisfiable) where

import Expr
import Control.Applicative              ( (<|>) )
import Control.DeepSeq                  (NFData(..), force)
import Control.Parallel.Strategies      (parMap, rpar, rseq, runEval, rdeepseq)
import qualified Data.Set as Set
import           Data.Maybe               (mapMaybe)
import Data.Set (Set)

instance NFData Expr where
  rnf (Var v)       = rnf v
  rnf (Const b)     = rnf b
  rnf (Fun f as)    = rnf f `seq` rnf as
  rnf (Pred p as)   = rnf p `seq` rnf as
  rnf (Equal e1 e2) = rnf e1 `seq` rnf e2
  rnf (Not e)       = rnf e
  rnf (And e1 e2)   = rnf e1 `seq` rnf e2
  rnf (Or  e1 e2)   = rnf e1 `seq` rnf e2

data Polarity = Positive | Negative deriving (Eq, Show)
instance NFData Polarity where rnf p = p `seq` ()

type Assignment = (Var, Bool)

collectPolarity :: Expr -> (Set Var, Set Var)
collectPolarity = go Set.empty Set.empty
  where
    go pos neg (Var v)       = (Set.insert v pos, neg)
    go pos neg (Not (Var v)) = (pos, Set.insert v neg)
    go pos neg (Not e)       = go pos neg e
    go pos neg (And e1 e2) =
      let (p1, n1) = go pos neg e1
          (p2, n2) = go p1 n1  e2
      in  (p2, n2)
    go pos neg (Or e1 e2)  =
      let (p1, n1) = go pos neg e1
          (p2, n2) = go p1 n1  e2
      in  (p2, n2)
    go pos neg _             = (pos, neg)

literalElimination :: Expr -> Expr
literalElimination expr = applyAll expr
  where
    (pos, neg)   = collectPolarity expr
    purePos      = pos Set.\\ neg
    pureNeg      = neg Set.\\ pos
    assignments  =  [(v, True)  | v <- Set.toList purePos]
                 ++ [(v, False) | v <- Set.toList pureNeg]
    substFns     = parMap rdeepseq (uncurry substVar) assignments
    applyAll     = foldl (.) id substFns

unitClause :: Expr -> Maybe Assignment
unitClause (Var v)       = Just (v, True)
unitClause (Not (Var v)) = Just (v, False)
unitClause _             = Nothing

clauses :: Expr -> [Expr]
clauses (And e1 e2) = clauses e1 ++ clauses e2
clauses e           = [e]

allUnitClauses :: Expr -> [Assignment]
allUnitClauses e = mapMaybe unitClause (clauses e)

unitPropagation :: Expr -> Expr
unitPropagation expr = applyAll expr
  where
    assignments = allUnitClauses expr
    substFns    = parMap rdeepseq (uncurry substVar) assignments
    applyAll    = foldl (.) id substFns

firstVarDPLL :: Expr -> Maybe Var
firstVarDPLL (Var v)       = Just v
firstVarDPLL (Const _)     = Nothing
firstVarDPLL (Fun _ _)     = Nothing
firstVarDPLL (Pred _ _)    = Nothing
firstVarDPLL (Equal _ _)   = Nothing
firstVarDPLL (Not e)       = firstVarDPLL e
firstVarDPLL (And e1 e2)   = firstVarDPLL e1 <|> firstVarDPLL e2
firstVarDPLL (Or  e1 e2)   = firstVarDPLL e1 <|> firstVarDPLL e2

-- Modificato per restituire (Bool, RecursiveCalls, DecisionSteps)
satisfiable :: Expr -> (Bool, Int, Int)
satisfiable = sat
  where
    sat :: Expr -> (Bool, Int, Int)
    sat e =
      let e' = literalElimination $ unitPropagation e
          recCalls = 1 -- chiamo questa funzione, incremento contatore
      in case firstVarDPLL e' of
           Nothing -> (unwrap (simplify e'), recCalls, 0) -- no decision step
           Just v  ->
             let !l = simplify (substVar v True  e')
                 !r = simplify (substVar v False e')
                 (resL, callsL, decL) = sat l
                 (resR, callsR, decR) = sat r
                 totalCalls = recCalls + callsL + callsR
                 totalDecisions = 1 + decL + decR
             in (resL || resR, totalCalls, totalDecisions)

    unwrap (Const b) = b
    unwrap _         = error "unwrap failed"
