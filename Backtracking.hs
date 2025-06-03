{-# LANGUAGE Trustworthy #-}

module Backtracking (satisfiable) where

import Expr
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Data.List (maximumBy)
import Data.Ord (comparing)

-- Cambiato per restituire (Bool, RecursiveCalls, DecisionSteps)
satisfiable :: Expr -> (Bool, Int, Int)
satisfiable = backtrack . simplify . convertToCnf

backtrack :: Expr -> (Bool, Int, Int)
backtrack expr =
  let recCalls = 1
  in case simplify expr of
    Const b -> (b, recCalls, 0)
    simplified ->
      case propagate simplified of
        Just e' ->
          let (res, calls, dec) = backtrack e'
          in (res, recCalls + calls, dec)
        Nothing ->
          case mostConstrainedVar simplified of
            Nothing -> (isTriviallyTrue simplified, recCalls, 0)
            Just v  ->
              let eTrue  = simplify (substVar v True simplified)
                  eFalse = simplify (substVar v False simplified)
                  (resT, callsT, decT) = backtrack eTrue
                  (resF, callsF, decF) = backtrack eFalse
                  totalCalls = recCalls + callsT + callsF
                  totalDecisions = 1 + decT + decF
              in (resT || resF, totalCalls, totalDecisions)

propagate :: Expr -> Maybe Expr
propagate expr =
  case findUnitClause expr of
    Just (v, val) -> propagate (substVar v val expr)
    Nothing ->
      case findPureLiteral expr of
        Just (v, val) -> propagate (substVar v val expr)
        Nothing -> Nothing

findUnitClause :: Expr -> Maybe (Var, Bool)
findUnitClause (Or (Var v) (Const False)) = Just (v, True)
findUnitClause (Or (Const False) (Var v)) = Just (v, True)
findUnitClause (Or (Not (Var v)) (Const False)) = Just (v, False)
findUnitClause (Or (Const False) (Not (Var v))) = Just (v, False)
findUnitClause (And a b) = findUnitClause a <|> findUnitClause b
findUnitClause _ = Nothing

findPureLiteral :: Expr -> Maybe (Var, Bool)
findPureLiteral e =
  let (pos, neg) = collectPolarities e Set.empty Set.empty
      purePos = Set.difference pos neg
      pureNeg = Set.difference neg pos
  in  if not (Set.null purePos)
        then Just (Set.findMin purePos, True)
        else if not (Set.null pureNeg)
               then Just (Set.findMin pureNeg, False)
               else Nothing

collectPolarities :: Expr -> Set.Set Var -> Set.Set Var -> (Set.Set Var, Set.Set Var)
collectPolarities (Var v) pos neg = (Set.insert v pos, neg)
collectPolarities (Not (Var v)) pos neg = (pos, Set.insert v neg)
collectPolarities (Not e) pos neg = collectPolarities e pos neg
collectPolarities (And a b) pos neg =
  let (pos1, neg1) = collectPolarities a pos neg
  in collectPolarities b pos1 neg1
collectPolarities (Or a b) pos neg =
  let (pos1, neg1) = collectPolarities a pos neg
  in collectPolarities b pos1 neg1
collectPolarities _ pos neg = (pos, neg)

mostConstrainedVar :: Expr -> Maybe Var
mostConstrainedVar expr =
  let freqMap = countVars expr Map.empty
  in if Map.null freqMap then Nothing
     else Just $ fst $ maximumByValue freqMap

countVars :: Expr -> Map.Map Var Int -> Map.Map Var Int
countVars (Var v) m = Map.insertWith (+) v 1 m
countVars (Not (Var v)) m = Map.insertWith (+) v 1 m
countVars (Not e) m = countVars e m
countVars (And a b) m = countVars b (countVars a m)
countVars (Or a b) m = countVars b (countVars a m)
countVars _ m = m

maximumByValue :: (Ord k, Ord v) => Map.Map k v -> (k, v)
maximumByValue = maximumBy (comparing snd) . Map.toList

isTriviallyTrue :: Expr -> Bool
isTriviallyTrue (Const True) = True
isTriviallyTrue _ = False
