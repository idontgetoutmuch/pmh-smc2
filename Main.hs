{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import           Data.Massiv.Array
import           Data.Random hiding (Normal)
import qualified Data.Random as R
import           System.Random hiding (uniform)
import qualified Data.Random.Source.MWC as MWC
import qualified Data.Vector as V
import           Data.Random.Source.PureMT
import           Data.IORef
import qualified System.Random.Stateful as RS
import           Data.Random.Distribution.MultivariateNormal
import qualified Numeric.LinearAlgebra.HMatrix as LA
import           Numeric.LinearAlgebra.HMatrix ( (><), Herm )
import           Control.Monad.State
import           Prelude hiding (read)
import           Graphics.Vega.VegaLite hiding ( sample, Normal )


import Debug.Trace

bigT = 500

deltaT = 0.01
g  = 9.81

qc1 = 0.0001

bigQL = [[qc1 * deltaT^3 / 3, qc1 * deltaT^2 / 2],
         [qc1 * deltaT^2 / 2,       qc1 * deltaT]]

bigQ :: Array U Ix2 Double
bigQ = fromLists' Par bigQL

bigQH :: LA.Herm Double
bigQH = LA.sym $ (2 >< 2) (concat bigQL)

bigRL = [[0.01]]

bigR :: Array U Ix2 Double
bigR  = fromLists' Par bigRL

bigRH :: LA.Herm Double
bigRH = LA.sym $ (1><1) (concat bigRL)

-- for t = 2:T+1
--     epsilon = rand(MvNormal(zeros(1), bigR));
--     y[t - 1, :] = sin(x[t- 1, 1]) .+ epsilon
--     x1 = x[t - 1, 1] + x[t - 1, 2] * deltaT;
--     x2 = x[t - 1, 2] - g * sin(x[t - 1, 1]) * deltaT;
--     eta = rand(MvNormal(zeros(2), bigQ));
--     xNew = [x1, x2] .+ eta;
--     x[t, :] = xNew;
-- end

-- for t = 2:T+1
--     x1 = x[t - 1, 1] + x[t - 1, 2] * deltaT;
--     x2 = x[t - 1, 2] - g * sin(x[t - 1, 1]) * deltaT;
--     xNew = [x1, x2];
--     x[t, :] = xNew;
-- end

simulatedDataPrim :: forall m . (MonadRandom m, PrimMonad m) =>
                     Double -> Double -> Ix1 -> m (Array P Ix2 Double)
simulatedDataPrim g deltaT bigT =
  createArrayS_ (Sz (bigT :. 2)) $ \ma -> foldlM_ (f ma) (0.01, 0.00) (0 ..: bigT)
  where
    f :: MArray (PrimState m) P Ix2 Double -> (Double, Double) -> Ix1 -> m (Double, Double)
    f ma a@(x1Prev, x2Prev) i = do
      eta <- sample (Normal (LA.vector [0.0, 0.0]) bigQH)
      let x1New = x1Prev + x2Prev * deltaT + eta LA.! 0
          x2New = x2Prev - g * sin x1Prev * deltaT + eta LA.! 1
      _ <- write ma (i :. 0) x1New
      _ <- write ma (i :. 1) x2New
      pure (x1New, x2New)

ts :: [Double]
ts = Prelude.map Prelude.fromIntegral [0 .. bigT]

enc = encoding
      . position X [ PName "a", PmType Quantitative ]
      . position Y [ PName "b", PmType Quantitative ]

main :: IO ()
main = do
  xs <- simulatedDataPrim g deltaT bigT
  let ys = fmap (!!0) $ toLists xs
      dat = dataFromColumns []
            . dataColumn "a" (Numbers ts)
            . dataColumn "b" (Numbers ys)
  toHtmlFile "bar.hmtl" $ toVegaLite [ dat [], mark Line [], enc [] ]
