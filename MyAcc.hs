{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Data.Array.Accelerate                       as A   hiding ( (^), Floating, (!!), V2, V3 )
import           Data.Array.Accelerate.LLVM.Native           as CPU
import           Data.Array.Accelerate.System.Random.MWC
import qualified Data.Array.Accelerate.Numeric.LinearAlgebra as ALA
import qualified Linear                                      as L
import           Data.Array.Accelerate.Linear                       hiding ( trace, (><) )
import           Data.Array.Accelerate.Control.Lens                 hiding ( use )
import qualified Numeric.LinearAlgebra as LA
import           Numeric.LinearAlgebra ( (><), (#>) )
import           Data.Random                                 as T   hiding ( uniform, normal )
import           Control.Monad.State
import qualified Data.Random.Distribution.Exponential        as T
import qualified Data.Random.Distribution.Poisson            as T
import qualified Data.Random.Distribution.Normal             as T

import qualified Language.R as S
import Language.R.QQ

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

exponential
    :: (Distribution StdUniform e, Floating e, Shape sh, Elt e)
    => e
    -> sh :~> e
exponential beta _sh gen = sampleFrom gen (T.exponential beta)

normal :: (Distribution T.Normal e, Shape sh, Elt e)
       => e -> e
       -> sh :~> e
normal mu sigma _sh gen = sampleFrom gen (T.normal mu sigma)

poisson
    :: (Distribution (T.Poisson b) a, Shape sh, Elt a)
    => b
    -> sh :~> a
poisson lambda _sh gen = sampleFrom gen (T.poisson lambda)

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
  s1 <- sampleFrom gen (T.normal (0.0 :: Double) 1.0)
  s2 <- sampleFrom gen (T.normal (0.0 :: Double) 1.0)
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

f :: V3 Double -> V3 Double -> V3 Double
f prev _ = V3 x1New x2New yNew
  where
    x1Prev = prev ^. L._x
    x2Prev = prev ^. L._y
    yPrev  = prev ^. L._z
    x1New  = x1Prev + x2Prev * deltaT
    x2New  = x2Prev - g * sin(x1Prev) * deltaT
    yNew   = sin(x1Prev)

test :: IO ()
test = do
  gen <- create
  bar <- biNormal (fromList (Z:.2) [0.0, 0.0]) bigQ gen
  print $ run bar
  return ()

oneStepH98 :: Double -> V2 (V2 Double) -> V2 (V2 Double)
oneStepH98 hh prev = V2 qNew pNew
  where
    h2 = hh / 2
    hhs = V2 hh hh
    hh2s = V2 h2 h2
    pp2 = psPrev - hh2s * nablaQ' qsPrev
    qNew = qsPrev + hhs * nablaP' pp2
    pNew = pp2 - hh2s * nablaQ' qNew
    qsPrev = prev ^. L._x
    psPrev = prev ^. L._y
    nablaQ' qs = V2 (qq1 / r) (qq2 / r)
      where
        qq1 = qs ^. L._x
        qq2 = qs ^. L._y
        r   = (qq1 ^ 2 + qq2 ^ 2) ** (3/2)
    nablaP' ps = ps

test1 :: IO ()
test1 = S.runRegion $ do
  _ <- [r| plot(1:10, c(3, 1, 5, 2, 3, 8, 4, 7, 6, 9), type = "l") |]
  return ()
