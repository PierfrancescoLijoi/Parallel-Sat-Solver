---------------------------------------------------------------------
-- CNF.hs
--
-- Parser per file DIMACS CNF che restituisce un'espressione logica
--    Expr in forma CNF.  Il parser scarta correttamente commenti, la
--    problem‑line, la riga terminatrice "0" e qualunque clausola
--    vuota, evitando cosí l'eccezione «foldr1: empty list».
---------------------------------------------------------------------
{-# LANGUAGE Trustworthy #-}

module CNF
  ( parseCNFFile   -- :: FilePath -> IO Expr
  , parseCNF       -- :: String   -> [[Expr]]
  ) where

import Expr                     -- Var, Expr(..)
import Data.Char      (isSpace)
import Data.Maybe     (mapMaybe)
import Control.Monad  (when)

---------------------------------------------------------------------
-- API ad alto livello ------------------------------------------------
---------------------------------------------------------------------

-- | Legge un file DIMACS CNF e lo converte in un'espressione logica
--   in forma congiuntiva normale (And di Or di letterali).
--   Solleva un'eccezione se non è presente alcuna clausola valida.
parseCNFFile :: FilePath -> IO Expr
parseCNFFile path = do
  contents <- readFile path
  let clauses = parseCNF contents
  when (null clauses) $
    ioError (userError "Il file CNF non contiene clausole valide")
  -- Converte [[l₁,l₂,…], …] ⇒ And (Or l₁ l₂ …) …
  pure (foldr1 And (map (foldr1 Or) clauses))

---------------------------------------------------------------------
-- Parser DIMACS di basso livello ------------------------------------
---------------------------------------------------------------------

-- | Estrae le clausole da una stringa contenente un file DIMACS CNF.
--   Restituisce una lista di clausole; ogni clausola è a sua volta
--   una lista di Expr (letterali).
parseCNF :: String -> [[Expr]]
parseCNF = mapMaybe parseClause               -- scarta Nothing
         . filter (not . skipLine)            -- rimuove commenti/altro
         . lines

---------------------------------------------------------------------
-- Funzioni di utilità ------------------------------------------------
---------------------------------------------------------------------

-- | Determina se la riga va ignorata (commento, problem‑line, ecc.).
skipLine :: String -> Bool
skipLine raw =
  let t = dropWhile isSpace raw               -- ignora spazi iniziali
  in  null t                   -- riga vuota
      || head t `elem` "c%"   -- commenti (c …) o %
      || head t == 'p'         -- problem line (p cnf …)
      || t == "0"             -- riga terminatrice singolo 0

-- | Converte una riga in clausola.
--   Se dopo aver rimosso lo 0 finale non rimangono letterali, ritorna
--   Nothing così la clausola verrà scartata.
parseClause :: String -> Maybe [Expr]
parseClause l =
  let tokens = takeWhile (/= "0") (words l)  -- tronca al primo 0
  in if null tokens
        then Nothing
        else Just (map parseLiteral tokens)

-- | Converte un letterale (stringa) in Expr.
--   "-12" ⇒ Not (Var "12") ; "7" ⇒ Var "7".
parseLiteral :: String -> Expr
parseLiteral ('-':xs) = Not (Var xs)
parseLiteral xs       = Var xs
