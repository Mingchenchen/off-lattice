{-# LANGUAGE RankNTypes, ScopedTypeVariables #-} 

module OffLattice.Util where


ljPotential :: Floating a
            => a -- depth of potential well
            -> a -- distance at which potential reaches its minimum
            -> a -- distance
            -> a
ljPotential e rm r = e * ((rm/r)**12 - 2 * (rm/r)**6)


expQuota :: Floating a
         => a
         -> a 
         -> a 
         -> a
expQuota x y t = exp ((-x+y) / t)


generateTemps :: forall n. Floating n => Int -> [n]
generateTemps n = [pf $ fromIntegral t | t <- [0..n]]
  where
    ef :: n -> n
    ef t = a * exp ((- t) * b / fromIntegral n)
    a :: n
    a = 1
    b :: n
    b = 1000
    pf :: n -> n
    pf t = a * (1 - t / fromIntegral n) ^ p
    p :: Int
    p = 2


