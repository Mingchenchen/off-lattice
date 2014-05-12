{-# LANGUAGE TypeFamilies, RankNTypes, ScopedTypeVariables,
    ViewPatterns, BangPatterns #-}


module OffLattice.HPChain where

import OffLattice.Util

import LA
import qualified LA.Transform as T
import qualified LA.Matrix as M

import qualified OffLattice.Chain as C
import qualified OffLattice.Shape as S
import qualified OffLattice.Geo as G

import qualified Data.Vector.Fixed as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG

import qualified Data.MonoTraversable as MT

import qualified Data.HashSet as HS

import qualified Data.Vector.Algorithms.Intro as VSORT


type Index = Int
type Id    = Int

type Angle n = n
type Radius n = n
type Potential n = n
type Energy n = n
type Range n  = n

type Vec n = F.ContVec F.N3 n
type Pos n = Vec n
type Bond n = (Index, Pos n, Vec n)

type Matrix n = Vec (Vec n)
type Transform n = (Matrix n, Vec n)

data HP = H | P
  deriving (Show, Read, Eq)

data HPN = H' | P' | N'
  deriving Eq

instance Show HPN where
  show H' = "H"
  show P' = "P"
  show N' = "N"

data HPConfig n = HPConfig { hRadius     :: Radius n
                           , pRadius     :: Radius n
                           , nRadius     :: Radius n
                           , hpPotential :: Potential n
                           , ppPotential :: Potential n
                           , hhPotential :: Potential n
                           , hpRange     :: Range n -- Interaction range
                           , bondAngle   :: Angle n
                           } deriving (Show, Read, Eq)

radius H' = hRadius
radius P' = pRadius
radius N' = nRadius

data HPChain n = HPChain { positions :: !(V.Vector (Pos n))
                         , residues  :: !(V.Vector HPN)
                         , bonds     :: !(V.Vector (Bond n))
                         , indices   :: !(V.Vector Index)
                         , config    :: !(HPConfig n)
                         }

instance forall n. Show n => Show (HPChain n) where
  show (HPChain ps rs bs is c) = "HPChain {" ++
    "\n  - positions:" ++ show ps' ++
    "\n  - residues:" ++ show (V.toList rs) ++
    "\n  - bonds:" ++ show bs' ++
    "\n  - indices:" ++ show (V.toList is) ++
    "\n  - config:" ++ show c

    where
      ps' :: [F.Tuple3 n]
      ps' = V.toList $ V.map F.convert ps
      bs' = V.toList $ V.map f bs
      
    
      f :: Bond n -> (Index, F.Tuple3 n, F.Tuple3 n)
      f (i, a, b) = (i, F.convert a, F.convert b)

      

instance ( Ord n
         , Additive n
         , Multiplicative n
         , Floating n
         , Epsilon n
         , Show n
         ) => C.Chain (HPChain n) where
  type Index (HPChain n)  = Index
  type Angle (HPChain n)  = Angle n
  type Energy (HPChain n) = Energy n
  
  indices = indices
  move = updateChain
  energy = energy


mkHPChain :: forall n.
             ( Additive n
             , Multiplicative n
             , Floating n
             , Epsilon n
             , Show n
             ) => [HP] -> HPConfig n -> HPChain n
mkHPChain !hps !hpc = HPChain ps rs bs is hpc
  where
    n  = length hps * 2
    m  = length hps
    !rs = V.fromList $ f hps
    !ps = V.generate n place
    !bs = V.generate (m-1) g
    !is = V.generate (m-1) id

    (c, s) = let a = bondAngle hpc / 2 in (cos a, sin a)
    
    place i = F.mk3 (s * x) (c * y) z
      where
        x = fromIntegral (i `div` 2)
        y | i `mod` 4 > 1  = 1
          | otherwise      = 0
        z | 1 <- i `mod` 4 = 1
          | 3 <- i `mod` 4 = (-1)
          | otherwise      = 0

    f [] = []
    f (H:xs) = N' : H' : f xs
    f (P:xs) = N' : P' : f xs
  
    g i | j <- 2*i = (j, place j, normalize $ place (2 + j) .- place j) 
    

rotChain :: forall n.
            ( Additive n
            , Multiplicative n
            , Floating n
            , Epsilon n
            ) => Index -> Angle n -> HPChain n -> HPChain n
rotChain _ a hpc | a ~= 0 = hpc
rotChain i a (HPChain !ps !rs !bs !is !hpc) | i >= V.length is || i < 0 = error "Index out of bounds"
                                        | otherwise = HPChain ps' rs bs' is hpc
  where
    (!j,!p,!b) = bs V.! i
    !bs' = V.imap f bs
    !ps' = V.imap g ps
    !lng = V.length bs `div` 2 < i || True

    f j (!k, !p, !b) | j <= i = (k, p, b)
                     | j >  i = (k, T.vtmulG p t, M.vmmulG b r)
    
    --g i r | i <= j  = r
    --      | i >  j  = T.vtmulG r t

    g !i !r | i <= j + 1 = r
            | i >  j + 1 = T.vtmulG r t

    !t  = T.rotAboutG p b a    :: Transform n
    !r  = T.rot3 b a           :: Matrix n

{-
updateChain :: forall n.
               ( Ord n
               , Additive n
               , Multiplicative n
               , Floating n
               , Epsilon n
               ) => Int -> Angle n -> HPChain n -> Maybe (HPChain n)
updateChain i a c | valid     = Just c'
                  | otherwise = Nothing
  where
    valid = validShapes s

    c'  = rotChain i a c
    rs  = residues c'
    ps  = positions c
    ps' = positions c'

    hpc = config c

    s = VG.izipWith f ps (positions c')
   
    f i a b = let r = radius (rs V.! i) hpc
              in if r /= 0 then S.capsule i r a b else error "radius is 0!"
-}
updateChain :: forall n.
               ( Ord n
               , Additive n
               , Multiplicative n
               , Floating n
               , Epsilon n
               ) => Int -> Angle n -> HPChain n -> Maybe (HPChain n)
updateChain !i !a c@(HPChain !ps !rs !bs !is !hpc) | valid     = Just c'
                                                   | otherwise = Nothing
  where
    !valid = validShapes2 s (j+1)

    (!j,!_,!_) = bonds c V.! i
    !c'  = rotChain i a c
    !rs  = residues c'
    !ps  = positions c
    !ps' = positions c'

    hpc = config c

    !s = VG.izipWith f ps (positions c')
   
    f !i !a !b = let r = radius (rs V.! i) hpc
                 in if r /= 0 then S.capsule i r a b else error "radius is 0!"

validShapes :: forall n.
               ( Ord n
               , Additive n
               , Multiplicative n
               , Epsilon n
               , Floating n
               ) => V.Vector (S.Shape Id (Vec n)) -> Bool
validShapes !s = MT.oall intersects overlaps
  where
    intervals :: Vec (V.Vector (G.Point n Id))
    !intervals = F.map (V.modify VSORT.sort) $ G.intervals s
    !overlaps  = HS.filter (\(i,j) -> abs (i-j) > 1) $ G.overlappings intervals
    intersects (!i,!j) = maybe True (const False) $ G.intersects (s V.! i) (s V.! j)


validShapes2 :: forall n.
               ( Ord n
               , Additive n
               , Multiplicative n
               , Epsilon n
               , Floating n
               ) => V.Vector (S.Shape Id (Vec n)) -> Int -> Bool
validShapes2 !s !i = MT.oall intersects overlaps
  where
    (!a,!b) = V.splitAt i s
    is, js :: Vec (V.Vector (G.Point n Id))
    !is = F.map (V.modify VSORT.sort) $ G.intervals a
    !js = F.map (V.modify VSORT.sort) $ G.intervals b
    !overlaps  = HS.filter (\(i,j) -> abs (i-j) > 1) $ G.overlappings2 is js
    intersects (!i,!j) = maybe True (const False) $ G.intersects (s V.! i) (s V.! j)


energy :: forall n.
          ( Ord n
          , Additive n
          , Multiplicative n
          , Epsilon n
          , Floating n
          , Show n
          ) => HPChain n -> Energy n
energy !ch@(HPChain !ps !rs !bs !is !hpc) = MT.ofoldr ((+) . enrgy) 0 overlaps
  where
    !rng = hpRange $ hpc
    !pp  = ppPotential $ hpc
    !hp  = hpPotential $ hpc
    !hh  = hhPotential $ hpc

    !xs = V.map f $ V.filter (isHP . snd) $ V.indexed rs

    f (!i, !_) = if rng /= 0 then S.Sphere rng (ps V.! i) i else error "rng == 0"

    isHP N' = False
    isHP _  = True

    intervals :: Vec (V.Vector (G.Point n Id))
    !intervals = F.map (V.modify VSORT.sort) $ G.intervals xs
    !overlaps = G.overlappings intervals
    
    enrgy (!i,!j) = let !d = dist (ps V.! i) (ps V.! j)
                        !e = if d >= rng then (0,0) else pot i j d
                    in fst e

    pot !i !j !d = let e H' H' = hh
                       e H' P' = hp
                       e P' H' = hp
                       e P' P' = pp
                       p = e (rs V.! i) (rs V.! j)
                   in (min 0 $ ljPotential p 1 d, p)


 
