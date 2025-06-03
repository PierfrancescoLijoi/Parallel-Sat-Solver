module Expr where

import Control.Applicative ((<|>))

-- Tipo variabile (nome)
type Var = String

-- Espressione booleana o predicati, uguaglianze e funzioni
data Expr
  = Var Var                  -- variabile atomica
  | Const Bool               -- costante booleana
  | Fun String [Expr]        -- funzione con argomenti
  | Pred String [Expr]       -- predicato con argomenti
  | Equal Expr Expr          -- uguaglianza tra espressioni
  | Not Expr                 -- negazione logica
  | And Expr Expr            -- congiunzione binaria
  | Or Expr Expr             -- disgiunzione binaria
  deriving (Eq, Show)

-- Trova la prima variabile libera nell'espressione
firstVar :: Expr -> Maybe Var
firstVar (Var v) = Just v
firstVar (Const _) = Nothing
firstVar (Fun _ args) = firstVarList args
firstVar (Pred _ _) = Nothing -- trattiamo predicati come atomi
firstVar (Equal _ _) = Nothing
firstVar (Not e) = firstVar e
firstVar (And e1 e2) = firstVar e1 <|> firstVar e2
firstVar (Or e1 e2) = firstVar e1 <|> firstVar e2

firstVarList :: [Expr] -> Maybe Var
firstVarList [] = Nothing
firstVarList (e:es) = firstVar e <|> firstVarList es

-- Sostituisce variabile con valore booleano (solo su Var)
substVar :: Var -> Bool -> Expr -> Expr
substVar v b (Var u) = if u == v then Const b else Var u
substVar _ _ c@(Const _) = c
substVar v b (Fun f args) = Fun f (map (substVar v b) args)
substVar v b (Pred p args) = Pred p (map (substVar v b) args)
substVar v b (Equal e1 e2) = Equal (substVar v b e1) (substVar v b e2)
substVar v b (Not e) = Not (substVar v b e)
substVar v b (And e1 e2) = And (substVar v b e1) (substVar v b e2)
substVar v b (Or e1 e2) = Or (substVar v b e1) (substVar v b e2)

-- Semplifica l'espressione booleana (non modifica predicati, funzioni, uguaglianze)
simplify :: Expr -> Expr
simplify (Var v) = Var v
simplify (Const b) = Const b
simplify (Fun f args) = Fun f (map simplify args)
simplify (Pred p args) = Pred p (map simplify args)
simplify (Equal e1 e2) = Equal (simplify e1) (simplify e2)
simplify (Not e) = case simplify e of
    Const b -> Const (not b)
    e' -> Not e'
simplify (And e1 e2) = case (simplify e1, simplify e2) of
    (Const False, _) -> Const False
    (_, Const False) -> Const False
    (Const True, e'') -> e''
    (e', Const True) -> e'
    (e', e'') -> And e' e''
simplify (Or e1 e2) = case (simplify e1, simplify e2) of
    (Const True, _) -> Const True
    (_, Const True) -> Const True
    (Const False, e'') -> e''
    (e', Const False) -> e'
    (e', e'') -> Or e' e''

-- Espande le negazioni usando leggi di De Morgan e doppia negazione
unfoldNot :: Expr -> Expr
unfoldNot (Not (Const b)) = Const (not b)
unfoldNot (Not (Not e)) = unfoldNot e
unfoldNot (Not (And e1 e2)) = Or (unfoldNot $ Not e1) (unfoldNot $ Not e2)
unfoldNot (Not (Or e1 e2)) = And (unfoldNot $ Not e1) (unfoldNot $ Not e2)
unfoldNot (Not e) = Not (unfoldNot e)
unfoldNot (And e1 e2) = And (unfoldNot e1) (unfoldNot e2)
unfoldNot (Or e1 e2) = Or (unfoldNot e1) (unfoldNot e2)
unfoldNot e = e

-- Distribuisce Or su And (per la CNF)
distribute :: Expr -> Expr
distribute (Or e1 (And e2 e3)) = And (distribute (Or e1 e2)) (distribute (Or e1 e3))
distribute (Or (And e1 e2) e3) = And (distribute (Or e1 e3)) (distribute (Or e2 e3))
distribute (Not e) = Not (distribute e)
distribute (And e1 e2) = And (distribute e1) (distribute e2)
distribute (Or e1 e2) = Or (distribute e1) (distribute e2)
distribute e = e

-- Converte l'espressione in CNF
convertToCnf :: Expr -> Expr
convertToCnf e =
  let e' = distribute (unfoldNot e)
  in if e == e' then e else convertToCnf e'