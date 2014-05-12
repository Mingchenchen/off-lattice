{-# LANGUAGE FlexibleContexts, TypeFamilies, RankNTypes,
    MultiParamTypeClasses, ScopedTypeVariables,
    UndecidableInstances, FlexibleInstances, BangPatterns #-}
    

module OffLattice.Shape ( Shape (..)
                        , capsule
                        ) where


import Prelude hiding (head, tail, foldl, foldl1, foldr, map, replicate, id, and)

import LA
import OffLattice.Geo as Geo
import Data.Vector.Fixed
import Data.Vector.Fixed.Generic



-- Capsule-Sphere shape
data Shape i a = Sphere { radius :: !(Scalar a)
                        , center :: !a
                        , sId    :: !i
                        }
               | Capsule { radius :: !(Scalar a)
                         , ends   :: !(a, a)
                         , sId    :: !i
                         }


-- Id instance
instance Id (Shape i a) where
  type I (Shape i a) = i
  id = sId

instance (Show (Scalar a), Show a, Show i) => Show (Shape i a) where
  show (Sphere r c i)  = "Sphere { radius = " ++ show r ++ 
    ", center = " ++ show c ++ ", sId = " ++ show i ++ " }"
  show (Capsule r e i) = "Capsule { radius = " ++ show r ++
    ", ends = " ++ show e ++ ", sId = " ++ show i ++ " }" 

instance ( InnerSpace v
         , Floating (Scalar v), Additive (Scalar v)
         , Ord (Scalar v), Epsilon (Scalar v)
         ) => Intersectable (Shape i v) where
  type Result (Shape i v) = Maybe (i,i)
   
  intersects (Sphere !r !c !i) (Sphere !r' !c' !i') = case r + r' >= dist c c' of
    True  -> Just (i, i')
    False -> Nothing
  intersects (Capsule !r !e !i) (Capsule !r' !e' !i') = case r + r' >= lineLineDist e e' of
    True  -> Just (i, i')
    False -> Nothing 
  intersects (Sphere !r !c !i) (Capsule !r' !e !i') = case r + r' >= pointLineDist c e of
    True  -> Just (i, i')
    False -> Nothing
  intersects a@(Capsule _ _ _) b@(Sphere _ _ _) = intersects b a


 
instance forall v a n i.
         ( Vector v a, Dim v ~ n
         , VectorSpace (v a), Scalar (v a) ~ a
         , Additive a, Ord a
         ) => AABB n (Shape i (v a)) where
  type P (Shape i (v a)) = a
  aabb (Sphere !r !c !i) = let !x = mapG (\a -> interval (a.-r) (a.+r) i) c in x
  aabb (Capsule !r !(e,e') !i) = let !x = mapG (\(a,b) -> interval (a.-r) (b.+r) i) c in x
    where
      c :: ContVec n (a,a)
      !c = zipWithG (\a b -> (min a b,max a b)) e e'


capsule :: forall v i n m.
           ( InnerSpace v
           , Epsilon (Scalar v)
           ) => i -> Scalar v -> v -> v -> Shape i v
capsule !i !r !a !b | eq        = Sphere r a i
                    | otherwise = Capsule r (a, b) i
  where
    !eq = closeToZero $ distSq a b


-- Capsule-Sphere utils, distance between line segs and points and such
------------------------------------------------------------------------
type LineSeg v = (v, v)

lineLineDist :: forall v. ( InnerSpace v
                          , Floating (Scalar v), Additive (Scalar v)
                          , Ord (Scalar v), Epsilon (Scalar v)
                          ) => LineSeg v -> LineSeg v -> Scalar v
lineLineDist !(p, q) !(r, s) = norm dP
  where
    !(u,v,w)       = (p.-q, s.-r, p.-r)
    !(a,b,c,d,e)   = (u*.*u, u*.*v, v*.*v, u*.*w, v*.*w)
    !d'            = a*c - b*b
    !(sN,sD,tN,tD) = g . f $ (0, d', 0, d')
    
    !sc | sN ~= 0 = 0
        | otherwise = sN / sD
    !tc | tN ~= 0 = 0
        | otherwise = tN / tD

    !dP = w .+ (u .% sc) .- (v .% tc)

    f (sN,sD,tN,tD) | d' ~= 0        = (0, 1, e, c)
                    | b*e - c*d < 0  = (0, sD, e, c)
                    | b*e - c*d > sD = (sD, sD, e + b, c)
                    | otherwise      = (b*e - c*d, sD, a*e - b*d, tD)

    g (sN,sD,tN,tD) | tN < 0 && (-d) < 0 = (0, sD, 0, tD)
                    | tN < 0 && (-d) > a = (sD, sD, 0, tD)
                    | tN < 0             = (-d, a, 0, tD)
                    | tN > tD && b-d < 0 = (0, sD, tD, tD)
                    | tN > tD && b-d > a = (sD, sD, tD, tD)
                    | tN > tD            = (b-d, a, tD, tD)
                    | otherwise          = (sN, sD, tN, tD)

pointLineDist :: ( InnerSpace v
                 , Floating (Scalar v), Additive (Scalar v)
                 , Ord (Scalar v), Epsilon (Scalar v)
                 ) => v -> LineSeg v -> Scalar v
pointLineDist p (v, w) | l ~= 0    = dist p w
                       | otherwise = d
  where
    !l = distSq v w
    !t = ((p.-v) *.* (w.-v)) / l
    !d | t < 0 = dist p v
       | t > 1 = dist p w
       | otherwise = dist p (v .+ ((w .- v) .% t))


























