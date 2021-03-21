{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}

import Data.Array.Accelerate              as A hiding ( (^), Floating )
import Data.Array.Accelerate.LLVM.Native  as CPU
import Data.Array.Accelerate.System.Random.MWC
import Data.Random                                    hiding ( uniform, normal )
import qualified Data.Random.Distribution.Exponential as R
import qualified Data.Random.Distribution.Poisson     as R
import qualified Data.Random.Distribution.Normal      as R


dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

exponential
    :: (Distribution StdUniform e, Floating e, Shape sh, Elt e)
    => e
    -> sh :~> e
exponential beta _sh gen = sampleFrom gen (R.exponential beta)

normal :: (Distribution R.Normal e, Shape sh, Elt e)
       => e -> e
       -> sh :~> e
normal mu sigma _sh gen = sampleFrom gen (R.normal mu sigma)

poisson
    :: (Distribution (R.Poisson b) a, Shape sh, Elt a)
    => b
    -> sh :~> a
poisson lambda _sh gen = sampleFrom gen (R.poisson lambda)

bigT = 500

deltaT = 0.01
g  = 9.81

qc1 = 0.0001

bigQ :: Matrix Double
bigQ = fromList (Z :. 2 :. 2)
       [ qc1 * deltaT^3 / 3, qc1 * deltaT^2 / 2
       , qc1 * deltaT^2 / 2, qc1 * deltaT
       ]

bigR :: Matrix Double
bigR = fromList (Z :. 1 :. 1)
       [0.01]
