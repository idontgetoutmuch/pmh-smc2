{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import           Data.Massiv.Array hiding ( S, L )
import qualified Data.Massiv.Array as M
import           Data.Random hiding ( Normal )
import qualified Data.Random as R
import           System.Random hiding ( uniform )
import qualified Data.Vector as V
import           Data.IORef
import qualified System.Random.Stateful as RS
import           Data.Random.Distribution.MultivariateNormal
import qualified Numeric.LinearAlgebra.HMatrix as LA
import           Numeric.LinearAlgebra.HMatrix ( (><), Herm )
import           Control.Monad.State
import qualified Control.Monad.State.Strict as SS
import           Prelude hiding ( read )
import qualified Prelude as P
import           Graphics.Vega.VegaLite hiding ( sample, Normal )
import qualified Graphics.Vega.VegaLite as VL
import           Data.Text ( Text(..), pack )

import Control.Monad.Reader
import Control.Monad.ST
import Debug.Trace
import qualified System.Random.MWC as MWC
import Control.Monad.Primitive


bigT = 500
bigN = 50

g, deltaT :: Double
deltaT = 0.01
g  = 9.81

qc1 = 0.0001

bigQL = [[qc1 * deltaT^3 / 3, qc1 * deltaT^2 / 2],
         [qc1 * deltaT^2 / 2,       qc1 * deltaT]]

bigQ :: Array U Ix2 Double
bigQ = fromLists' Par bigQL

bigQH :: LA.Herm Double
bigQH = LA.sym $ (2 >< 2) (concat bigQL)

bigRL = [[0.0001]]

bigR :: Array U Ix2 Double
bigR  = fromLists' Par bigRL

bigRH :: LA.Herm Double
bigRH = LA.sym $ (1><1) (concat bigRL)

simulatedDataPrim :: forall g m . (StatefulGen g m, PrimMonad m) =>
                     Double -> Double -> Ix1 -> g -> m (Array P Ix2 Double)
simulatedDataPrim g deltaT bigT stateGen =
  createArrayS_ (Sz (bigT :. 3)) $ \ma -> foldlM_ (f ma) (0.01, 0.00, sin 0.01 + 0.0) (0 ..: bigT)
  where
    f :: MArray (PrimState m) P Ix2 Double -> (Double, Double, Double) -> Ix1 -> m (Double, Double, Double)
    f ma a@(x1Prev, x2Prev, _) i = do
      eta <- sampleFrom stateGen (Normal (LA.vector [0.0, 0.0]) bigQH)
      let x1New = x1Prev + x2Prev * deltaT + eta LA.! 0
          x2New = x2Prev - g * sin x1Prev * deltaT + eta LA.! 1
      _ <- write ma (i :. 0) x1New
      _ <- write ma (i :. 1) x2New
      epsilon <- sampleFrom stateGen (Normal (LA.vector [0.0]) bigRH)
      let y = sin x1New + (epsilon LA.! 0)
      _ <- write ma (i :. 2) y
      pure (x1New, x2New, y)

simulatedDataPrim' :: forall g m . (StatefulGen g m, PrimMonad m, MonadReader g m) =>
                     Double -> Double -> Ix1 -> m (Array P Ix2 Double)
simulatedDataPrim' g deltaT bigT =
  createArrayS_ (Sz (bigT :. 3)) $ \ma -> foldlM_ (f ma) (0.01, 0.00, sin 0.01 + 0.0) (0 ..: bigT)
  where
    f :: MArray (PrimState m) P Ix2 Double -> (Double, Double, Double) -> Ix1 -> m (Double, Double, Double)
    f ma a@(x1Prev, x2Prev, _) i = do
      eta <- sample (Normal (LA.vector [0.0, 0.0]) bigQH)
      let x1New = x1Prev + x2Prev * deltaT + eta LA.! 0
          x2New = x2Prev - g * sin x1Prev * deltaT + eta LA.! 1
      _ <- write ma (i :. 0) x1New
      _ <- write ma (i :. 1) x2New
      epsilon <- sample (Normal (LA.vector [0.0]) bigRH)
      let y = sin x1New + (epsilon LA.! 0)
      _ <- write ma (i :. 2) y
      pure (x1New, x2New, y)

-- FIXME: Also return log weights array
-- FIXME: Maybe use just map rather than e.g. !+!
-- FIXME: Generalise with a state update function and an observation function
pfPrim :: forall g m . (StatefulGen g m, PrimMonad m, MonadReader g m, MonadThrow m) =>
          Array P Ix2 Double ->
          Double -> Double -> Ix1 -> Ix1 ->
          Array P Ix1 Double -> m (Array P Ix3 Double)
pfPrim inits g deltaT bigT bigN ys = do
  let initWeights = fromList Seq $ Prelude.replicate bigN (1.0 / fromIntegral bigN)
  createArrayS_ (Sz (bigT :> bigN :. 2)) $
    \ma -> foldlM_ (f ma) (inits, initWeights) (0 ..: bigT)
  where
    f ma (aOld, wOld) i = do
      js <- resample_stratified' wOld
      let z_pf = computeAs P $
                 backpermute' (Sz (2 :. bigN)) (\(i :. j) -> i :. (js!j)) aOld
      etas <- replicateM bigN $ sample (Normal (LA.vector [0.0, 0.0]) bigQH)
      let eta1s = fromList Seq $ Prelude.map (LA.! 0) etas
          eta2s = fromList Seq $ Prelude.map (LA.! 1) etas

      let z1Prev = computeAs P $ aOld !> 0
          z2Prev = computeAs P $ aOld !> 1
          z1New  = z1Prev !+! z2Prev .* deltaT !+! eta1s
          z2New  = z2Prev !-! (computeAs P $ M.map (\x -> g * sin x * deltaT) z1Prev) !+! eta2s
      _ <- zipWithM_ (\j x1New -> write ma (i :> j :. 0) x1New) [0 .. bigN - 1] (toList z1New)
      _ <- zipWithM_ (\j x2New -> write ma (i :> j :. 1) x2New) [0 .. bigN - 1] (toList z2New)
      let z3New = M.map sin z2New
      let logW = M.zipWith (\y x -> logPdf (Normal (LA.vector [y]) bigRH) (LA.vector [x])) ys z3New
          maxW = maximum' logW
          wPre = M.map exp $ logW .- maxW
          wNew = wPre ./ (M.sum wPre)

      aNew <- stackSlicesM (Dim 2) [z1New, z2New]
      pure (computeAs P aNew, computeAs P wNew)

-- See https://xianblog.wordpress.com/tag/stratified-resampling/
resample_stratified'
  :: forall g m . (PrimMonad m, StatefulGen g m, MonadReader g m) =>
     Array P Ix1 Double -> m (Array P Ix1 Int)
resample_stratified' weights = indices

  where

    bigN = elemsCount weights

    cumulative_sum :: PrimMonad m => m (Array P Ix1 Double)
    cumulative_sum = createArrayS_ (Sz bigN) $
      (\ma -> foldM_ (f ma) 0.0 (0 ..: bigN))
      where
        f ma s i = do
          let v = weights!i
              t = s + v
          _ <- write ma i t
          return t

    -- Make N subdivisions, and chose a random position within each one
    positions :: (StatefulGen g m, PrimMonad m) => m (Array P Ix1 Double)
    positions = createArrayS_ (Sz bigN) $
      \ma -> foldlM_ (f ma) 0.0 (0 ..: bigN)
      where
        f ma _ i = do
          epsilon <- sample (Uniform 0.0 1.0)
          let t = (epsilon + fromIntegral i) / (fromIntegral bigN)
          _ <- write ma i t
          return t

    indices :: (StatefulGen g m, PrimMonad m) => m (Array P Ix1 Int)
    indices = do
      ps <- positions
      cs <- cumulative_sum
      let f ma s i = do
            let go j =
                  if (ps!i) < (cs!j)
                  then do
                    _ <- write ma i j
                    return j
                  else go (j + 1)
            go s
      createArrayS_ (Sz bigN) $ \ma -> foldlM_ (f ma) 0 (0 ..: bigN)

ts :: [Double]
ts = Prelude.map Prelude.fromIntegral [0 .. bigT]

xs = fmap (!!0) $ toLists $
     RS.runSTGen_ (mkStdGen 42) (simulatedDataPrim g deltaT bigT)

ys = fmap (!!2) $ toLists $
     RS.runSTGen_ (mkStdGen 42) (simulatedDataPrim g deltaT bigT)

enc = encoding
      . position X [ PName "a", PmType Quantitative ]
      . position Y [ PName "b", PmType Quantitative ]

dat = dataFromColumns []
      . dataColumn "a" (Numbers ts)
      . dataColumn "b" (Numbers xs)

linePlot :: Text -> Text -> VL.VLSpec
linePlot xName yName =
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Quantitative]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Quantitative]
  in VL.asSpec [VL.mark VL.Line [VL.MColor "blue"], encoding []]

pointPlot :: Text -> Text -> VL.VLSpec
pointPlot xName yName =
  let encoding = VL.encoding
            . VL.position VL.X [VL.PName xName, VL.PmType VL.Quantitative]
            . VL.position VL.Y [VL.PName yName, VL.PmType VL.Quantitative]
  in VL.asSpec [VL.mark VL.Circle [VL.MColor "red"], encoding []]


data SpecGrid = H [[VL.VLSpec]] | V [[VL.VLSpec]] | L [VL.VLSpec] | S VL.VLSpec | F (Text, Int, VL.VLSpec)

data InputData = Cols [(Text, VL.DataValues)]
               | File FilePath

plot :: (Double, Double) -> SpecGrid -> InputData -> VL.VegaLite
plot (figw,figh) specGrid dataPoints =
    let description = VL.description "Plot"
        dat' = case dataPoints of
            Cols cols -> foldl (.) (VL.dataFromColumns []) (P.map (uncurry VL.dataColumn) cols) []
            File fp -> VL.dataFromSource (pack fp) []
        configure = VL.configure
            . VL.configuration (VL.Axis
                                        [ VL.Domain False,
                                          VL.LabelColor "#7F7F7F",
                                          VL.LabelPadding 4,
                                          VL.TickColor "#7F7F7F",
                                          VL.TickSize 5.67,
                                          VL.Grid True,
                                          VL.GridColor "#FFFFFF"
                                          ])
        spec = case specGrid of
            S s -> VL.layer [s]
            L ls -> VL.layer ls
            H lss -> VL.hConcat (P.map (VL.asSpec . (:[]) . VL.layer) lss)
            V lss -> VL.vConcat (P.map (VL.asSpec . (:[]) . VL.layer) lss)
            F (_, _, s) -> VL.specification s
        facet = case specGrid of
            F (field, nColumns, _) -> [VL.columns $ fromIntegral nColumns, VL.facetFlow [VL.FName field, VL.FmType VL.Nominal]]
            _   -> [VL.width figw,  VL.height figh]
    in VL.toVegaLite $ [VL.background "#f9f9f9", configure [], description, dat', spec] ++ facet

inits :: Array P Ix2 Double
inits = fromLists' Seq [Prelude.replicate bigN 0.01, Prelude.replicate bigN 0.00]

test :: (MonadReader g m, MonadThrow m, StatefulGen g m, PrimMonad m) =>
        m (Array P Ix3 Double)
test = do
  ys <- simulatedDataPrim' g deltaT bigT
  let zs = computeAs P $ ys !> 0
  pfPrim inits g deltaT bigT bigN zs

main :: IO ()
main = do
  is :: Array P Ix1 Int <- MWC.create >>= runReaderT (resample_stratified' ((fromList Seq ([0.5] ++ P.replicate 5 0.1)) :: Array P Ix1 Double))
  print is
  toHtmlFile "bar.hmtl" $ toVegaLite [ dat [], mark Line [], enc [] ]
  toHtmlFile "baz.html" $
    plot (600, 300) (L [pointPlot "time" "angle", linePlot "time" "horizontal displacement"]) (Cols [("time", VL.Numbers ts), ("angle", VL.Numbers ys), ("horizontal displacement", VL.Numbers xs)])
  zs <- MWC.create >>= runReaderT test
  return ()
