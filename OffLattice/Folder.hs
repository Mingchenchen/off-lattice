{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}


module OffLattice.Folder where

import LA
import OffLattice.HPChain as HPChain
import OffLattice.Chain as Chain
import OffLattice.Metro as Metro
import OffLattice.Util as Util

import qualified Data.Vector.Fixed as F
import qualified Data.Vector as V

import Control.Monad (forM_)

import System.IO
import System.Directory (createDirectoryIfMissing)
import System.Environment
import System.Random.MWC

import qualified Data.List as L

import Data.Time
import Data.Time.Format
import System.Locale

getPath :: IO String
getPath = getZonedTime >>= return . formatTime defaultTimeLocale "output/%Y-%m-%d %H:%M:%S/"

{-
runNormal :: ( Additive n
             , Multiplicative n
             , Floating n
             , Variate n
             , Epsilon n
             , Ord n
             , Show (HPChain.Energy n)
             ) => Int -> Int -> n -> [HP] -> HPConfig n -> IO ()
runNormal atmpts its angle hps conf = do
  g <- createSystemRandom
  let hpc = mkHPChain hps conf

  path <- getPath

  createDirectoryIfMissing True folderName
  writeFile (path ++ "initialChain.csv") $ showChain hpc ++ "\n" ++ toHPN hps
  writeFile (path ++ "output/initialEnergy.csv") $ show (HPChain.energy hpc)
  r <- runChain g atmpts angle hpc (generateTemps its )
  writeFile (path ++ "energies.csv") $ (
  putStrLn $ "Stats:\n\t" ++ (show $ rStats r)
  putStrLn $ "Energy before: " ++ (show $ HPChain.energy $ hpc)
  putStrLn $ "Energy after:  " ++ (show $ HPChain.energy $ rState r)
-}


runLarge :: forall n.
            ( Additive n
            , Multiplicative n
            , Floating n
            , Variate n
            , Epsilon n
            , Ord n
            , Show n
            ) => Int -> Int -> n -> [HP] -> HPConfig n -> IO ()
runLarge n its angle hps conf = do
  g <- createSystemRandom
  let hpc = mkHPChain hps conf
  path <- getPath

  createDirectoryIfMissing True path
  createDirectoryIfMissing True (path ++ "chains")
  createDirectoryIfMissing True (path ++ "hpchains")
  energyHandle <- openFile (path ++ "energies.csv") WriteMode
  statsHandle  <- openFile (path ++ "stats.csv") WriteMode
  writeFile (path ++ "residues.csv") (concatMap show hps)
  writeFile (path ++ "iterations") $ show its
  writeFile (path ++ "config") $ show conf
  writeFile (path ++ "initialChain.csv") $ showChain hpc ++ "\n" ++ toHPN hps
  writeFile (path ++ "initialEnergy.csv") $ (show $ HPChain.energy hpc)

  let writeOut i res = do {
    writeFile (path ++ "chains/chain-" ++ show i ++ ".csv") (showResult res) ;
    writeFile (path ++ "hpchains/chain-" ++ show i ++ ".csv") (showResult res ++ toHPN hps) ;
    hPutStrLn energyHandle (show $ HPChain.energy $ rState res) ;
    hPutStrLn statsHandle (showStats $ rStats $ res) ;
    hFlush energyHandle ;
    hFlush statsHandle ;
    putStrLn $ "Finished number " ++ show i ++ " of " ++ show n ;
  }

  let runfunc i = do
                    r <- runChain g angle hpc (generateTemps $ its)
                    writeOut i r

  forM_ [1..n] runfunc
  hClose energyHandle
  hClose statsHandle
  putStrLn "Save results in folder output"

showResult :: forall n. (Show n, Ord n, Num n) => Result (HPChain n) -> String
showResult = showChain . rState

showChain :: forall n. (Show n, Ord n, Num n) => HPChain n -> String
showChain r = unlines $ map f $ ps
  where
    ps :: [(n, n, n)]
    ps = map F.convert $ V.toList $ positions $ r
    lz = min 0 $ minimum $ map (\(_,_,c) -> c) ps
    ly = min 0 $ minimum $ map (\(_,b,_) -> b) ps
    lx = min 0 $ minimum $ map (\(a,_,_) -> a) ps
    f (a,b,c) = show (a-lx) ++ " " ++ show (b-ly) ++ " " ++ show (c-lz)

showStats :: Stats -> String
showStats  = show


toHPN [] = []
toHPN (H:xs) = 'N' : ' ' : 'H' : ' ' : toHPN xs
toHPN (P:xs) = 'N' : ' ' : 'P' : ' ' : toHPN xs
























