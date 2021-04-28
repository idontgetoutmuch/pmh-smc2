{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wall              #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main
  ( main
  , ys
  , xs
  ) where

import           Data.Massiv.Array hiding ( S, L )
import qualified Data.Massiv.Array as M
import           Data.Random hiding ( Normal )
import           System.Random hiding ( uniform )
import qualified System.Random.Stateful as RS
import           Data.Random.Distribution.MultivariateNormal
import qualified Numeric.LinearAlgebra.HMatrix as LA
import           Numeric.LinearAlgebra.HMatrix ( (><) )
import           Control.Monad.State
import           Prelude hiding ( read )
import qualified Prelude as P

import qualified Language.R as R
import           Language.R.QQ

import           Control.Monad.Reader
import Debug.Trace
import qualified System.Random.MWC as MWC

bigT, bigN :: Int
bigT = 500
bigN = 50

g, deltaT :: Double
deltaT = 0.01
g  = 9.81

qc1 :: Double
qc1 = 0.0001

bigQL :: [[Double]]
bigQL = [[qc1 * deltaT^3 / 3, qc1 * deltaT^2 / 2],
         [qc1 * deltaT^2/ 2,  qc1 * deltaT      ]]

bigQ :: Array U Ix2 Double
bigQ = fromLists' Par bigQL

bigQH :: LA.Herm Double
bigQH = LA.sym $ (2 >< 2) (concat bigQL)

bigRL :: [[Double]]
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

stateUpdate aPerm = do
  etas <- replicateM bigN $ sample (Normal (LA.vector [0.0, 0.0]) bigQH)
  let eta1s = fromList Seq $ Prelude.map (LA.! 0) etas
      eta2s = fromList Seq $ Prelude.map (LA.! 1) etas

  let z1Prev = computeAs P $ aPerm !> 0
      z2Prev = computeAs P $ aPerm !> 1
      z1New  = z1Prev !+! z2Prev .* deltaT !+! eta1s
      z2New  = z2Prev !-! (computeAs P $ M.map (\x -> g * sin x * deltaT) z1Prev) !+! eta2s
  aNew <- stackSlicesM (Dim 2) [z1New, z2New]
  return aNew

measure a y = (w, logW)
  where
    z1 = computeAs P $ a !> 0
    z3 = M.map sin z1
    logW :: Array D Ix1 Double
    logW = M.map (\x -> logPdf (Normal (LA.vector [y]) bigRH) (LA.vector [x])) z3
    maxW = maximum' logW
    wPre = M.map exp $ logW .- maxW
    w = wPre ./ (M.sum wPre)

-- FIXME: Maybe use just map rather than e.g. !+!
-- FIXME: Generalise with a state update function and an observation function
-- FIXME: Can we avoid zipWithM?
pfPrim :: forall g m . (StatefulGen g m, PrimMonad m, MonadReader g m, MonadThrow m) =>
          Array P Ix2 Double ->
          Double -> Double -> Ix1 -> Ix1 -> Ix1 ->
          (Array P Ix2 Double -> m (Array DL Ix2 Double))->
          (Array P Ix2 Double -> Double -> (Array D Ix1 Double, Array D Ix1 Double)) ->
          Array P Ix1 Double ->
          m ((Array P Ix2 Double, Array P Ix1 Double, Int, Double), Array P Ix3 Double)
pfPrim inits g deltaT bigT bigN bigP stateUpdate measure ys = do
  let initWeights = fromList Seq $ Prelude.replicate bigN (1.0 / fromIntegral bigN)
  createArrayS (Sz (bigT :> bigN :. (bigP + 1))) $
    \ma -> foldlM (f ma) (inits, initWeights, 0, 0.0) ys
  where
    f ma (aOld, wOld, i, oldLogW) y = do
      js <- resample_stratified wOld
      let aPerm = computeAs P $
                  backpermute' (Sz (bigP :. bigN)) (\(i :. j) -> i :. (js!j)) aOld
      aNew <- computeAs P <$> stateUpdate aPerm

      M.mapM_ (\(j :. k) -> write ma (i :> j :. k) (aNew ! (j :. k))) (0 :. 0 ... (bigP - 1) :. (bigN - 1))

      let (wNew', logW') = measure aNew y

      -- FIXME: For now store the log weights in the time / particle array (maybe forever)
      _ <- zipWithM_ (\j lw -> write ma (i :> j :. bigP) lw) [0 .. bigN - 1] (toList logW')

      let newLogW :: Double
          newLogW = oldLogW + log ((/ fromIntegral bigN) $ M.sum $ M.map exp logW')

      pure (computeAs P aNew, computeAs P wNew', i + 1, newLogW)

priorMu = 9.0

priorPdf ::
            Double -> Double
priorPdf theta =
  pdf (Normal (LA.vector [priorMu]) (LA.sym $ (1><1) [1.0])) (LA.vector [theta])

priorSample :: forall g m . (StatefulGen g m, MonadReader g m) =>
               m (LA.Vector Double)
priorSample = sample (Normal (LA.vector [priorMu]) (LA.sym $ (1><1) [1.0]))

pmh :: forall g m . (MonadReader g m, StatefulGen g m, PrimMonad m, MonadThrow m) =>
     Array P Ix2 Double
     -> Ix1 -> Ix1 -> Ix1 -> Ix1
     -> Array P Ix1 Double
     -> Double
     -> m Double
     -> (Double -> Double)
     -> (Array P Ix2 Double -> m (Array DL Ix2 Double))
     -> (Array P Ix2 Double -> Double -> (Array D Ix1 Double, Array D Ix1 Double))
     -> m ((MArray (PrimState m) P Ix1 Double, Double, Double), Array P Ix4 Double)
pmh inits bigK bigN bigT bigP ys deltaT priorSample priorPdf stateUpdate measure = do
  initTheta <- priorSample
  ((_, _, _, initlogWeight), _) <- pfPrim inits initTheta deltaT bigT bigN bigP stateUpdate measure ys
  initThetas <- newMArray (Sz bigK) (0.0 / 0.0)
  createArrayS (Sz (bigP :> bigN :> bigT :. bigK)) $
    \ma -> foldlM (f ma) (initThetas, initTheta, initlogWeight) (makeVectorR D Seq (Sz bigK) id)
  where
    f ma (mTheta, thetaOld, logWeightOld) l = do
      eta <- priorSample
      let thetaProp = exp (log thetaOld + 0.05 * eta)
      ((_, _, _, logWeightProp), particles) <- pfPrim inits thetaProp deltaT bigT bigN 2 stateUpdate measure ys
      Prelude.mapM_ (\(i, j, k) -> write ma (i :> j :> k :. l) (particles ! (k :> j :. i)))
                    [(i, j, k) | i <- [0 .. bigP - 1],
                                 j <- [0 .. bigN - 1],
                                 k <- [0 .. bigT - 1]]

      let mhRatio = exp (logWeightProp - logWeightOld) * priorPdf thetaProp / priorPdf thetaOld
          alpha = if isNaN mhRatio
                  then 0.0
                  else min 1.0 mhRatio
      dm <- sample StdUniform
      if dm < alpha
        then do write mTheta l thetaProp
                return $ (mTheta, thetaProp, logWeightProp)
        else do write mTheta l thetaOld
                return (mTheta, thetaOld,  logWeightOld)

smc2 :: forall g m . (MonadReader g m, StatefulGen g m, PrimMonad m, MonadThrow m) =>
     Array P Ix2 Double
     -> Ix1 -> Ix1 -> Ix1 -> Ix1
     -> Array P Ix1 Double
     -> Double
     -> m Double
     -> (Double -> Double)
     -> (Array P Ix2 Double -> m (Array DL Ix2 Double))
     -> (Array P Ix2 Double -> Double -> (Array D Ix1 Double, Array D Ix1 Double))
     -> m ((MArray (PrimState m) P Ix1 Double, Double, Double), Array P Ix4 Double)
smc2 inits bigK bigN bigT bigP ys deltaT priorSample priorPdf stateUpdate measure = do
  -- x_th(:,:,1) = prior_sample(N_th);
  return undefined
  where
    f ma s l = do
      return undefined


-- See https://xianblog.wordpress.com/tag/stratified-resampling/
resample_stratified
  :: forall g m . (PrimMonad m, StatefulGen g m, MonadReader g m) =>
     Array P Ix1 Double -> m (Array P Ix1 Int)
resample_stratified weights = indices

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

xs :: [Double]
xs = fmap (!!0) $ toLists $
     RS.runSTGen_ (mkStdGen 42) (simulatedDataPrim g deltaT bigT)

ys :: [Double]
ys = fmap (!!2) $ toLists $
     RS.runSTGen_ (mkStdGen 42) (simulatedDataPrim g deltaT bigT)

inits :: Array P Ix2 Double
inits = fromLists' Seq [Prelude.replicate bigN 0.01, Prelude.replicate bigN 0.00]

test :: (MonadReader g m, MonadThrow m, StatefulGen g m, PrimMonad m) =>
        m (Array P Ix2 Double, Array P Ix3 Double)
test = do
  ds <- simulatedDataPrim' g deltaT bigT
  let zs = computeAs P $ (transpose ds) !> 2
  (_, us) <- pfPrim inits g deltaT bigT bigN 2 stateUpdate measure zs
  return (ds, us)

bigK :: Int
bigK = 100

testPmh :: forall g m . (MonadReader g m, MonadThrow m, StatefulGen g m, PrimMonad m) =>
           m ((MArray (PrimState m) P Ix1 Double, Double, Double), Array P Ix4 Double)
testPmh = do
  ds <- simulatedDataPrim' g deltaT bigT
  let zs = computeAs P $ (transpose ds) !> 2
  pmh inits bigK bigN bigT 2 zs deltaT (liftM (LA.! 0) priorSample) priorPdf stateUpdate measure

main :: IO ()
main = do
  h <- MWC.create
  ((mb, _thetaFin, _logWeightFinal), _ma) <- runReaderT testPmh h
  mc <- freeze Seq mb
  let cs = toList mc
  R.runRegion $ do
    _  <- [r| library(ggplot2) |]
    df <- [r| data <- data.frame(cs_hs) |]
    p1 <- [r| ggplot(data=df_hs, aes(x=cs_hs)) |]
    _  <- [r| p1_hs + geom_histogram()         |]
    _  <- [r| ggsave(filename="diagrams/thetaHistza.png") |]
    return ()

  is <- runReaderT (resample_stratified (fromList Seq ([0.5] ++ P.replicate 5 0.1))) h
  print is
  (as, zs) <- MWC.create >>= runReaderT test
  let ws = Prelude.take bigT ts
  let us = Prelude.map (/ fromIntegral bigN) $
           Prelude.map (\i -> M.sum $ (transposeOuter zs) !> i !> 0) $
           Prelude.take bigT [0..]
  let bs = Prelude.map (\i -> as !> i !> 0) $
           Prelude.take bigT [0..]
  let ds = Prelude.map (\i -> as !> i !> 2) $
           Prelude.take bigT [0..]
  R.runRegion $ do
    _ <- [r| print(file.path(R.home("bin"), "R")) |]
    _ <- [r| library(ggplot2) |]
    df <- [r| data <- data.frame(ws_hs, us_hs, bs_hs) |]

    p1 <- [r| ggplot(df_hs, aes(x=ws_hs)) |]
    p2 <- [r| p1_hs + geom_line(aes(y = us_hs), color = "darkred") |]
    p3 <- [r| p2_hs + geom_line(aes(y = bs_hs), color="steelblue") |]
    _  <- [r| p3_hs + geom_point(aes(y = ds_hs), color = "red") |]
    _  <- [r| ggsave(filename="diagrams/viaR.png") |]
    return ()
  return ()
