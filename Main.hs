{-# LANGUAGE Trustworthy #-}

module Main where

import qualified DPLL as DPLLMod
import qualified Backtracking as BTMod

import Expr                        (convertToCnf, Expr)
import CNF                         (parseCNFFile)
import System.Environment          (getArgs)
import System.FilePath             ((</>))
import System.Directory            (createDirectoryIfMissing)
import System.IO                   (withFile, IOMode(..), hPutStrLn)
import Text.Printf                 (printf)
import Data.List                   (intercalate, isPrefixOf)

import Control.DeepSeq             (force)
import Control.Exception           (evaluate)
import qualified Data.Map.Strict as Map
import GHC.Stats                   (getRTSStats, gcdetails_allocated_bytes, RTSStats(..))
import System.Mem                  (performGC)
import Control.Monad               (forM, forM_)

import System.Clock                (Clock(Monotonic), getTime, toNanoSecs)

---------------------------------------------------------------------
-- Helpers -----------------------------------------------------------

groupName :: FilePath -> String
groupName f
  | "uf20"  `isPrefixOf` base = "uf20"
  | "uf50"  `isPrefixOf` base = "uf50"
  | "uf75"  `isPrefixOf` base = "uf75"
  | otherwise                 = "Other"
  where
    base = reverse . takeWhile (/= '/') . reverse $ f

groupProperties :: String -> (Int, Int, Int, Int)
groupProperties "uf20"  = (1000, 3, 20, 91)
groupProperties "uf50"  = (1000, 3, 50, 218)
groupProperties "uf75"  = (100,  3, 75, 325)
groupProperties _       = (0, 0, 0, 0)

formatBytesKB :: Double -> String
formatBytesKB b = printf "%.2f" (b / 1024)

timeAction :: IO a -> IO (Double, a)
timeAction action = do
  start <- getTime Monotonic
  result <- action
  end <- getTime Monotonic
  let diffNano = fromIntegral (toNanoSecs end - toNanoSecs start) :: Double
      diffSeconds = diffNano / 1e9
  return (diffSeconds, result)

---------------------------------------------------------------------
-- Funzione che esegue il solver e raccoglie TUTTE le statistiche ----

runSolverCollectAllStats :: (Expr -> (Bool, Int, Int)) -> Expr -> IO (Bool, Int, Int, Double, Double)
runSolverCollectAllStats solver expr = do
  performGC  -- Clean heap before timing
  beforeStats <- getRTSStats
  (timeTaken, (result, recCalls, decSteps)) <- timeAction $ evaluate . force $ solver expr -- Force full evaluation
  performGC -- Clean heap after timing
  afterStats <- getRTSStats 
  let allocBytes = fromIntegral
        (gcdetails_allocated_bytes (gc afterStats) - gcdetails_allocated_bytes (gc beforeStats))
  return (result, recCalls, decSteps, timeTaken, allocBytes)

---------------------------------------------------------------------
-- Main --------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  let uf20  = ["test/uf20-0" ++ show x ++ ".cnf" | x <- [1 .. 1000]]
      uf50  = ["test/uf50-0" ++ show x ++ ".cnf" | x <- [1 .. 1000]]
      -- uf75  = ["test/uf75-0" ++ show x ++ ".cnf" | x <- [1 .. 100]]

      defaultFiles = uf20 ++ uf50 -- ++ uf75
      testFiles = if null args then defaultFiles else args

  putStrLn $ "Reading CNF files: " ++ intercalate ", " testFiles

  exprs <- mapM parseCNFFile testFiles
  let cnfTests = map convertToCnf exprs

      grouped = zipWith (\fp e -> (groupName fp, (fp, e))) testFiles cnfTests
      groupMap = Map.fromListWith (++) [(g, [(fp, e)]) | (g, (fp, e)) <- grouped]
      allGroups = Map.toList groupMap

  -- Per ogni gruppo e file, esegui DPLL e raccogli statistiche
  dpllStatsPerGroup <- forM allGroups $ \(gName, pairs) -> do
    forM pairs $ \(fp, e) -> do
      putStrLn $ "DPLL solving " ++ fp
      runSolverCollectAllStats DPLLMod.satisfiable e >>= \ (r, rec, dec, t, a) ->
        pure (gName, fp, "DPLL", r, rec, dec, t, a)

  -- Per ogni gruppo e file, esegui Backtracking e raccogli statistiche
  btStatsPerGroup <- forM allGroups $ \(gName, pairs) -> do
    forM pairs $ \(fp, e) -> do
      putStrLn $ "Backtracking solving " ++ fp
      runSolverCollectAllStats BTMod.satisfiable e >>= \ (r, rec, dec, t, a) ->
        pure (gName, fp, "Backtracking", r, rec, dec, t, a)

  let dpllRows = concat dpllStatsPerGroup
      btRows = concat btStatsPerGroup

  -- --- Scrittura file solver_results.csv ---

  let outputDir = "result/"
  createDirectoryIfMissing True outputDir
  let resultFile = outputDir </> "Solver_Results.csv"

  -- Mappa proprietà gruppo per colonne addizionali
  let propsMap = Map.fromList [ (g, groupProperties g) | (g, _) <- allGroups ]

  -- Funzione per creare righe CSV result.csv
  let toResultCSVLine (grp, file, solver, res, _rec, _dec, time, alloc) =
        let (instances, clauseLen, vars, clauses) = Map.findWithDefault (0,0,0,0) grp propsMap
            resultStr = if res then "True" else "False"
        in intercalate "," [ grp
                           , file
                           , solver
                           , resultStr
                           , printf "%.9f" time
                           , formatBytesKB alloc
                           , show instances
                           , show clauseLen
                           , show vars
                           , show clauses
                           ]

  withFile resultFile WriteMode $ \h -> do
    hPutStrLn h "Group,File,Solver,Result,Time,AllocBytes(KB),instances,clause-len,vars,clauses"
    mapM_ (hPutStrLn h . toResultCSVLine) (dpllRows ++ btRows)

  putStrLn $ "Detailed per-file summary saved to: " ++ resultFile

  -- --- Scrittura file A.csv (recursive calls e decision steps) ---

  let aFile = outputDir </> "NumberofRecursiveCalls_DecisionSteps_Solver.csv"
  let toCallsCSVLine (grp, file, solver, _res, recCalls, decSteps, _time, _alloc) =
        intercalate "," [grp, file, solver, show recCalls, show decSteps]

  withFile aFile WriteMode $ \h -> do
    hPutStrLn h "Group,File,Solver,Number of Recursive Calls,Decision Steps"
    mapM_ (hPutStrLn h . toCallsCSVLine) (dpllRows ++ btRows)

  putStrLn $ "Calls statistics saved to: " ++ aFile

  -- --- Totals & summary ---

  let totalTime rows = sum [ t | (_, _, _, _, _, _, t, _) <- rows ]

      printRow name rows = do
        putStrLn $ name ++ " total time: " ++ printf "%.9f s" (totalTime rows)
        let groupedTime = Map.toList $ Map.fromListWith (+) [(g, t) | (g, _, _, _, _, _, t, _) <- rows]
        forM_ groupedTime $ \(g, t) ->
          putStrLn $ "  [" ++ g ++ "] " ++ printf "%.9f s" t

  putStrLn "\n--- Summary ---"
  printRow "DPLL" dpllRows
  printRow "Backtracking" btRows

  -- Controllo disaccordi (risultati booleani)
  let lookupRes m k = Map.lookup k m

      dpllMap = Map.fromList [ ((g, f), r) | (g, f, _, r, _, _, _, _) <- dpllRows ]
      btMap = Map.fromList [ ((g, f), r) | (g, f, _, r, _, _, _, _) <- btRows ]

      allFiles = Map.keys dpllMap ++ Map.keys btMap
      uniqueFiles = Map.keys $ Map.fromList [ (k, k) | k <- allFiles ]

      disagreements = [ (f, d, b)
                      | f <- uniqueFiles
                      , let d = lookupRes dpllMap f
                            b = lookupRes btMap f
                      , d /= b
                      , Just _ <- [d]
                      , Just _ <- [b]
                      ]

  if null disagreements
    then putStrLn "\nAll solvers agree on all files."
    else do
      putStrLn "\nWARNING: disagreements:"
      forM_ disagreements $ \((grp, fp), Just d, Just b) ->
        putStrLn $ "  " ++ grp ++ "/" ++ fp ++ ": DPLL=" ++ show d ++ ", Backtracking=" ++ show b

  putStrLn "\nAll solvers finished."
