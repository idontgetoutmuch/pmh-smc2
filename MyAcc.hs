{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}

import           Data.Array.Accelerate                as A hiding ( (^), Floating, (!!) )
import           Data.Array.Accelerate.LLVM.Native    as CPU
import           Data.Array.Accelerate.System.Random.MWC
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as ALA
import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra ( (><), (#>) )
import           Data.Random                          as R hiding ( uniform, normal )
import           Control.Monad.State
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

bigQL :: [Double]
bigQL = [ qc1 * deltaT^3 / 3, qc1 * deltaT^2 / 2
        , qc1 * deltaT^2 / 2, qc1 * deltaT
        ]

bigQ :: Matrix Double
bigQ = fromList (Z :. 2 :. 2) bigQL

bigQH :: LA.Herm Double
bigQH = LA.sym $ (2 >< 2) bigQL

bigR :: Matrix Double
bigR = fromList (Z :. 1 :. 1)
       [0.01]

biNormal :: RandomSource m s =>
            Vector Double ->
            Matrix Double ->
            s ->
            m (Acc (Vector Double))
biNormal mu sigma gen = do
  s1 <- sampleFrom gen (R.normal (0.0 :: Double) 1.0)
  s2 <- sampleFrom gen (R.normal (0.0 :: Double) 1.0)
  return $ A.zipWith (+) (use mu)
                         (bigA ALA.#> (use $ fromList (Z:.2) [s1, s2]))

  where
    ys = toList sigma
    sigma1 = ys!!0
    rho12  = ys!!1
    sigma2 = ys!!3
    tr = sigma1 + sigma2
    det = sigma1 * sigma2 - rho12^2
    l1 = 0.5 * (tr + sqrt(tr^2 - 4 * det))
    l2 = 0.5 * (tr - sqrt(tr^2 - 4 * det))
    lSqrt = ALA.diagonal $ A.map sqrt $ use $ fromList (Z:.2) [l1, l2]
    n1 = sqrt $ rho12^2 + (l1 - sigma1)^2
    n2 = sqrt $ rho12^2 + (l2 - sigma1)^2
    bigU  = fromList (Z:.2:.2) $
            [rho12 / n1, (sigma1 - l1) / n1, rho12 / n2, (sigma1 - l2) / n2]
    bigA = (use bigU) ALA.<> lSqrt

a = qc1 * deltaT^3 / 3
b = qc1 * deltaT^2 / 2
c = qc1 * deltaT

test :: IO ()
test = do
  gen <- create
  bar <- biNormal (fromList (Z:.2) [0.0, 0.0]) bigQ gen
  print $ run bar
  return ()

normalMultivariate :: LA.Vector Double -> LA.Herm Double -> R.RVarT m (LA.Vector Double)
normalMultivariate mu bigSigma = do
  z <- replicateM (LA.size mu) (rvarT R.StdNormal)
  return $ mu + bigA LA.#> (LA.fromList z)
  where
    (vals, bigU) = LA.eigSH bigSigma
    lSqrt = LA.diag $ LA.cmap sqrt vals
    bigA = bigU LA.<> lSqrt
