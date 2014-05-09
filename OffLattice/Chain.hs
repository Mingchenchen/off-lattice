{-# LANGUAGE TypeFamilies, FlexibleContexts #-}


module OffLattice.Chain ( Chain (..)
                        , runChain
                        ) where


import LA (Additive, neg)
import OffLattice.Util as Util

import qualified OffLattice.Metro as M

import qualified Data.Vector as V

-- mwc-random stuff
import qualified System.Random.MWC as MWC
import Control.Monad.Primitive (PrimMonad, PrimState)


class Chain c where
  type Index c  :: *
  type Angle c  :: *
  type Energy c :: *

  indices :: c -> V.Vector (Index c)
  move    :: (Index c) -> (Angle c) -> c -> Maybe c
  energy  :: c -> Energy c

index :: ( PrimMonad m
         ) => MWC.Gen (PrimState m) -> V.Vector a -> m a
index g is = MWC.uniformR (0, n-1) g >>= return . (is V.!)
  where
    n = V.length is

angle :: ( PrimMonad m
         , Additive a
         , MWC.Variate a
         ) => MWC.Gen (PrimState m) -> a -> m a
angle g a = MWC.uniformR (a, neg a) g

candidate :: ( PrimMonad m
             , Chain c
             , Additive (Angle c)
             , MWC.Variate (Angle c)
             , Num n
             , Fractional n
             , Fractional (Angle c)
             , Ord n
             ) => MWC.Gen (PrimState m)
               -> Angle c
               -> c
               -> Int
               -> m (Maybe (M.Candidate c n))
candidate g a c n = do
  a' <- angle g a >>= return . (* (2/3)^(fromIntegral n))
  i  <- index g $ indices c
  case move i a' c of
    Nothing -> return Nothing
    (Just x) -> return $ Just $ M.Candidate x 1 1


score :: ( Chain c
         , n ~ Energy c
         , Floating n
         ) => c -> c -> n -> n
score a b = expQuota (energy a) (energy b)
     



type ScoreF c t n = c -> c -> t -> n
type CandidateF m c n = Angle c -> c -> m (Maybe (M.Candidate c n))

type Temperature n = n


runChain :: ( PrimMonad m
            , Chain c
            , Additive (Angle c)
            , MWC.Variate (Angle c)
            , Fractional (Angle c)
            , Ord t
            , Num t
            , Fractional t
            , Floating t
            , Energy c ~ t
            ) => MWC.Gen (PrimState m)
              -> Int 
              -> Angle c
              -> c
              -> [Temperature t]
              -> m (M.Result c)
runChain g atmpts angl = M.metropolisHastings g atmpts score (candidate g angl)

















