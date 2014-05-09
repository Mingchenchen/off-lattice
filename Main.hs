{-# LANGUAGE TypeFamilies, RankNTypes, ScopedTypeVariables #-}

module Main where

import System.Exit
import System.Environment

import LA
import OffLattice.HPChain as HPChain
import OffLattice.Folder as Folder

import Data.Char (toLower)
import Data.Maybe (isNothing)


import System.Random.MWC

type N = Double

data Settings = S { iterations :: Maybe Int
                  , large      :: Maybe Int
                  , chain      :: Maybe [HP]
                  , help       :: Maybe Bool
                  , angle      :: N
                  , attempts   :: Int
                  } deriving (Show, Read, Eq)
                  
settings0 = S { iterations = Nothing
              , large = Nothing
              , chain = Nothing
              , help = Nothing
              , angle = 15
              , attempts = 150
              }

defaultConf :: HPConfig N
defaultConf = HPConfig { hRadius = r
                       , pRadius = r
                       , nRadius = r
                       , hpPotential = 0
                       , ppPotential = 0
                       , hhPotential = 1
                       , hpRange = 2.5
                       , bondAngle = a
                       }
  where
    r = 0.49
    p = 1
    a = 90



main :: IO ()
main = do
    args <- getArgs
    case concat args of
      [] -> do
              printHelp
              exitSuccess
      _  -> return ()
    
    let (s,c) = parse (settings0, defaultConf) args
    case help s of
      (Just True) -> do
                       printHelp 
                       exitSuccess
      _           -> return ()
    case iterations s of
      Nothing -> do
                   putStrLn "You must set the number of iterations"
                   exitFailure
      _       -> return ()
    case chain s of
      Nothing -> do
                   putStrLn "You must set the hp-chain"
                   exitFailure
      _       -> return ()
    let (Just ch) = chain s
    let an = (angle s)/180 * pi
    let (Just it) = iterations s
    let at = attempts s
    let c' = c { bondAngle = (bondAngle c) / 180 * pi }
    case large s of
      (Just n) -> runLarge n at it an ch c'
      Nothing  -> runNormal  at it an ch c'
  


parse (s,c) [] = (s,c)
parse (s,c) ("-h":xs) = (s { help = Just True }, c)
parse (s,c) (x:[])    = (s,c)
parse (s,c) (x:y:xs) = case x of
  "-h" -> parse (s { help = Just True }, c) (y:xs)
  _    -> case lookup x dict of
            Nothing  -> parse (s,c) (y:xs)
            (Just f) -> parse (f (s,c) y) xs
    
parseChain [] = []
parseChain (x:xs) = case toLower x of
                      'h' -> H : parseChain xs
                      'p' -> P : parseChain xs
                      _   -> error "Could not parse chain" 

dict = [ ("-i" , \(s, c) y -> (s { iterations = Just (read y :: Int) }, c))
       , ("-c" , \(s, c) y -> (s { chain      = Just (parseChain y)  }, c))
       , ("-l" , \(s, c) y -> (s { large      = Just (read y :: Int) }, c))
       , ("-a" , \(s, c) y -> (s { angle      = (read y :: N)   }, c))
       , ("-at", \(s, c) y -> (s { attempts   = (read y :: Int) }, c))
       , ("-b" , \(s, c) y -> (s, c { bondAngle   = (read y :: N) }))
       , ("-d" , \(s, c) y -> (s, c { hpRange     = (read y :: N) }))
       , ("-pp", \(s, c) y -> (s, c { ppPotential = (read y :: N) }))
       , ("-hp", \(s, c) y -> (s, c { hpPotential = (read y :: N) }))
       , ("-hh", \(s, c) y -> (s, c { hhPotential = (read y :: N) }))
       , ("-hr", \(s, c) y -> (s, c { hRadius     = (read y :: N) }))
       , ("-pr", \(s, c) y -> (s, c { pRadius     = (read y :: N) }))
       , ("-nr", \(s, c) y -> (s, c { nRadius     = (read y :: N) }))
       ]

printHelp = do
    putStrLn $ "Flags:"
    putStrLn $ "  -h                          show this info"
    putStrLn $ "* -i <iterations>             set the number of iterations"  
    putStrLn $ "  -l <number of runs>         fold the chain multiple times"
    putStrLn $ "* -c <chain>                  set the hp-chain"
    putStrLn $ "  -a <angle>                  set the step anglel,"
    putStrLn $ "                                default value = " ++ (show $ angle settings0)
    putStrLn $ "  -at <attempts>               set the number of attempts,"
    putStrLn $ "                                default value = " ++ (show $ attempts settings0)
    putStrLn $ "  -b <angle>                  set the bond angle,"
    putStrLn $ "                                default value = " ++ (show $ bondAngle defaultConf)
    putStrLn $ "  -pp <potential>             set the potential of P-P interaction,"
    putStrLn $ "                                default value = " ++ (show $ ppPotential defaultConf)
    putStrLn $ "  -hh <potential>             set the potential of H-H interaction,"
    putStrLn $ "                                default value = " ++ (show $ hhPotential defaultConf)
    putStrLn $ "  -hp <potential>             set the potential of H-P interaction,"
    putStrLn $ "                                default value = " ++ (show $ hpPotential defaultConf)
    putStrLn $ "  -hr <radius>                set the radius of H residues,"
    putStrLn $ "                                default value = " ++ (show $ hRadius defaultConf)
    putStrLn $ "  -pr <radius>                set the radius of P residues,"
    putStrLn $ "                                default value = " ++ (show $ pRadius defaultConf)
    putStrLn $ "  -nr <radius>                set the radius of N residues,"
    putStrLn $ "                                default value = " ++ (show $ nRadius defaultConf)
    putStrLn $ "  -d <distance>               set the cut-off interaction distance,"
    putStrLn $ "                                default value = " ++ (show $ hpRange defaultConf)










