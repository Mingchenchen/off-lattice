{-# LANGUAGE FlexibleContexts, TypeFamilies, TemplateHaskell,
    MultiParamTypeClasses, RankNTypes, ScopedTypeVariables #-}
    

module OffLattice.Geo ( Id (..)
                       , Intersectable (..)
                       , AABB (..)
                       , interval
                       , intervals
                       , overlaps
                       , overlappings
                       , Point
                       ) where


import Prelude hiding (head, tail, foldl, foldl1, foldr, map, replicate, id)

-- Fixed vector stuff
import Data.Vector.Fixed as F
import Data.Vector.Fixed.Generic

-- Fold stuff
import Data.MonoTraversable

-- Data.Vector stuff
import qualified Data.Vector.Generic as G

-- HashSet stuff
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)

-- Unboxing stuff
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed.Base (Unbox)
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Generic.Base as VB

import Debug.Trace

-- Shape id
class Id a where
  type I a :: *
  id :: a -> I a


-- Intersectable
------------------------------------------------------------------------
class Intersectable a where
  type Result a
  intersects :: a -> a -> Result a


-- Point
------------------------------------------------------------------------
data Point p i = Open p i | Close p i
  deriving (Show, Read)

-- Unboxed instance stuff
pointToUTriple (Open  a i) = (a,i,True)
pointToUTriple (Close a i) = (a,i,False)

uTripleToPoint (a,i,True)  = Open a i
uTripleToPoint (a,i,False) = Close a i

derivingUnbox "Point"
  [t| (Unbox a, Unbox i) => Point a i -> (a,i,Bool) |]
  [| pointToUTriple |]
  [| uTripleToPoint |]


valP :: Point p i -> p
valP (Open  x _) = x
valP (Close x _) = x

idP :: Point p i -> i
idP (Open  _ x) = x
idP (Close _ x) = x

instance Eq i  => Eq (Point p i) where
  (==) a b = idP a == idP b

instance (Ord p, Eq i) => Ord (Point p i) where
  compare a b = case compare (valP a) (valP b) of
                  EQ -> if o a then GT else LT
                  x  -> x
    where
      o (Open _ _) = True
      o _          = False

instance Id (Point p i) where
  type I (Point p i) = i
  id = idP

-- Interval
------------------------------------------------------------------------
newtype Interval p i = Interval { points :: (Point p i, Point p i) }
  deriving (Show, Read, Eq)

derivingUnbox "Interval"
  [t| (Unbox p, Unbox i) => Interval p i -> (Point p i, Point p i) |]
  [| points |]
  [| Interval |]

{-
interval :: Ord p => p -> p -> i -> Interval p i
interval a b i | a < b  = Interval (Open a i, Close b i)
               | a >  b = Interval (Open b i, Close a i)
               | a == b = Interval (Open a i, Close b i)
-}            

interval :: Ord p => p -> p -> i -> Interval p i
interval a b i = case compare a b of
                   LT -> Interval (Open a i, Close b i)
                   GT -> Interval (Open b i, Close b i)
                   EQ -> Interval (Open a i, Close b i)
              


instance Id (Interval p i) where
  type I (Interval p i) = i
  id = id . fst . points

-- AABB
------------------------------------------------------------------------
class Id a => AABB n a where
  type P a :: *
  aabb :: forall v p i.
          ( P a ~ p, I a ~ i, F.Dim v ~ n
          , Vector v (Interval p i)
          ) => a -> v (Interval p i)

intervals :: forall n a v w p i.
             ( AABB n a
             , p ~ P a, i ~ I a, F.Dim v ~ n
             , G.Vector w (Point p i)
             , G.Vector w a
             , Vector v (w (Point p i))
             ) => w a -> v (w (Point p i))
intervals xs = vector $ generate (G.generate (2*n) . f)
  where
    n = G.length xs

    f :: Int -> Int -> Point p i
    f i j | even j
          , k <- j `div` 2 = fst $ points $ (g k) ! i
          | k <- j `div` 2 = snd $ points $ (g k) ! i

    g :: Int -> ContVec n (Interval p i)
    g i = aabb $ xs G.! i

-- Assumes that the points are sorted in ascending order
overlappings :: forall n v p i f.
               ( Hashable i, Ord i, Ord p, Arity n
               , MonoFoldable f
               , Element f ~ (Point p i)
               , Vector v f, F.Dim v ~ S n
               ) => v f -> HashSet (i,i)
overlappings xs = foldr (HS.intersection . overlaps) x xs'
  where
    x = (overlaps $ head xs)
    xs' :: ContVec n f
    xs' = tail xs


-- Assumes that the points are sorted in ascending order.
overlaps :: forall p i f.
            ( Hashable i, Ord i, Ord p
            , MonoFoldable f
            , Element f ~ (Point p i)
            ) => f -> HashSet (i,i)
overlaps = snd . ofoldl' (flip f) (HS.empty, HS.empty)
  where
    f (Open  p i) (is, r) = (HS.insert i is, HS.foldr (HS.insert . g i) r is)
    f (Close p i) (is, r) = (HS.delete i is, r) -- insert/delete is ~O(1)
    g i j = if i < j then (i,j) else (j,i)
{-
overlaps2 :: forall p i f.
             ( Hashable i, Ord i, Ord p
             , MonoFoldable f
             , Element f ~ (Point p i)
             ) => f -> f -> HashSet (i,i)
overlaps2 = 
--f xs ys is js cs
  f [] _  is _  cs | HS.null is = cs
  f _  [] _  js cs | HS.null js = cs
  f (x:xs) (y:ys) is js cs
    | x == y
    , op x   = f xs (y:ys) (ins x is) js (add x js cs)
    | x == y
    , op y   = f (x:xs) ys is (ins y js) (add y is cs)
    | x < y  
    , op x   = f xs (y:ys)
    | x < y  =
    | x >= y
    , op y   =
    | x >= y =
  where
    op (Open _ _) = True
    op _          = Close

    fo
-}







