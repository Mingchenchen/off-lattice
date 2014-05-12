{-# LANGUAGE BangPatterns, TupleSections, RankNTypes, ScopedTypeVariables #-}

module OffLattice.Metro ( Candidate (..)
                        , metropolisHastings
                        , Result (..)
                        , Stats (..)
                        , stats0
                        , rState
                        , rStats
                        ) where

import System.Random.MWC 
import Control.Monad
import Control.Monad.Primitive

-- A function that takes two states and one parameter and
-- calculates the quotient between the scores of the two
-- states given the parameter.
type ScoreFunc s p n = s -> s -> p -> n

--  A function that generates a new random candidate
-- given the current state of a Markov chain.
type CandFunc s m n = s -> m (Maybe (Candidate s n))

-- A structure containing the candidate state, the
-- probability of getting this state given the previous
-- state and the probability of getting the previous state
-- given this state.
data Candidate s n = Candidate { candidate :: s
                               , pthere :: n
                               , pback :: n
                              }

data Result a = Finished Stats a | GotStuckAt Stats a
  deriving (Show, Read, Eq)

rState :: Result a -> a
rState (Finished _ a) = a
rState (GotStuckAt _ a) = a

rStats :: Result a -> Stats
rStats (Finished s _) = s
rStats (GotStuckAt s _) = s

data Stats = Stats { attempts :: Int
                   , rejected :: Int
                   } deriving (Show, Read, Eq)

attempt (Stats i j) = Stats (i+1) j
reject  (Stats i j) = Stats i (j+1)
stats0 = Stats 0 0

metropolisHastings :: forall m n p s. 
                      ( PrimMonad m
                      , Ord n
                      , Num n
                      , Fractional n
                      ) => Gen (PrimState m)
                        -> ScoreFunc s p n
                        -> CandFunc s m n
                        -> s
                        -> [p]
                        -> m (Result s)
metropolisHastings g !scoref !candf !state !ts = foldM step (state, stats0, False) ts >>= f
  where
    f (!state, !stats, True)  = return $ GotStuckAt stats state
    f (!state, !stats, False) = return $ Finished stats state
 
    step :: (s, Stats, Bool) -> p -> m (s, Stats, Bool)
    step (!state, stats', !stck) !t = do
      --cand <- try atmpts state Nothing
      !cand <- candf state
      case cand of 
        Nothing -> return (state, stats', True) ;
        (Just c) -> let !candState    = candidate c 
                        (there,back) = (pthere c, pback c) 
                        score        = scoref candState state t
                        a            = min 1.0 back/there * score
                    in if a >= 1
                         then return (candState, stats', stck)
                         else return (state, reject stats', stck)





foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x:xs) = do
      z' <- f z x
      z' `seq` foldM' f z' xs






































