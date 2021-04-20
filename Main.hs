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

import Control.Monad.Reader
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
    \ma -> foldlM_ (f ma) (inits, initWeights, 0) ys
  where
    f ma (aOld, wOld, i) y = do
      js <- resample_stratified wOld
      let aPerm = computeAs P $
                  backpermute' (Sz (2 :. bigN)) (\(i :. j) -> i :. (js!j)) aOld
      etas <- replicateM bigN $ sample (Normal (LA.vector [0.0, 0.0]) bigQH)
      let eta1s = fromList Seq $ Prelude.map (LA.! 0) etas
          eta2s = fromList Seq $ Prelude.map (LA.! 1) etas

      let z1Prev = computeAs P $ aPerm !> 0
          z2Prev = computeAs P $ aPerm !> 1
          z1New  = z1Prev !+! z2Prev .* deltaT !+! eta1s
          z2New  = z2Prev !-! (computeAs P $ M.map (\x -> g * sin x * deltaT) z1Prev) !+! eta2s
      _ <- zipWithM_ (\j x1New -> write ma (i :> j :. 0) x1New) [0 .. bigN - 1] (toList z1New)
      _ <- zipWithM_ (\j x2New -> write ma (i :> j :. 1) x2New) [0 .. bigN - 1] (toList z2New)
      let z3New = M.map sin z1New
      let logW = M.map (\x -> logPdf (Normal (LA.vector [y]) bigRH) (LA.vector [x])) z3New
          maxW = maximum' logW
          wPre = M.map exp $ logW .- maxW
          wNew = wPre ./ (M.sum wPre)

      aNew <- stackSlicesM (Dim 2) [z1New, z2New]
      pure (computeAs P aNew, computeAs P wNew, i + 1)

-- function pmh(inits, K, N, n_th, y, f_g, g, nx, prior_sample, prior_pdf, Q, R)

--     T = length(y);
--     theta = zeros(n_th, K+1);
--     log_W = -Inf;
--     # FIXME:
--     x_pfs = zeros(nx, N, T, K);

--     while log_W == -Inf # Find an initial sample without numerical problems
--         theta[:, 1] = 9 .+ prior_sample(1);
--         # FIXME:
--         log_W = pf(inits, N, (x) -> f_g(x, theta[:, 1][1]), g, y, Q, R, nx)[3];
--     end

--     for k = 1:K
--         theta_prop = map(exp, map(log, theta[:, k]) + 0.1 * rand(MvNormal(zeros(n_th), 1), 1)[1, :]);
--         # log_W_prop = pf(inits, N, (x) -> f_g(x, theta_prop[1]), g, y, Q, R, nx)[3];
--         (a, b, c) = pf(inits, N, (x) -> f_g(x, theta_prop[1]), g, y, Q, R, nx);
--         log_W_prop = c;
--         x_pfs[:, :, :, k] = a;
--         mh_ratio = exp(log_W_prop - log_W) * prior_pdf(theta_prop) / prior_pdf(theta[:,k]);

--         # display([theta[:, k], theta_prop, log_W, log_W_prop, mh_ratio]);

--         if isnan(mh_ratio)
--             alpha = 0;
--         else
--             alpha = min(1,mh_ratio);
--         end

--         dm = rand();
--         if dm < alpha
--             theta[:, k+1] = theta_prop;
--             log_W = log_W_prop;
--             new = true;
--         else
--             theta[:, k+1] = theta[:, k];
--             new = false;
--         end

--         # if new == true;
--         #     display(["PMH Sampling ", k, ": Proposal accepted!"]);
--         # else
--         #     display(["PMH Sampling ", k, ": Proposal rejected"]);
--         # end
--     end
--     return (x_pfs, theta);
-- end

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

xs = fmap (!!0) $ toLists $
     RS.runSTGen_ (mkStdGen 42) (simulatedDataPrim g deltaT bigT)

ys = fmap (!!2) $ toLists $
     RS.runSTGen_ (mkStdGen 42) (simulatedDataPrim g deltaT bigT)

inits :: Array P Ix2 Double
inits = fromLists' Seq [Prelude.replicate bigN 0.01, Prelude.replicate bigN 0.00]

test :: (MonadReader g m, MonadThrow m, StatefulGen g m, PrimMonad m) =>
        m (Array P Ix2 Double, Array P Ix3 Double)
test = do
  ds <- simulatedDataPrim' g deltaT bigT
  let zs = computeAs P $ (transpose ds) !> 2
  us <- pfPrim inits g deltaT bigT bigN zs
  return (ds, us)

main :: IO ()
main = do
  h <- MWC.create
  is <- runReaderT (resample_stratified (fromList Seq ([0.5] ++ P.replicate 5 0.1))) h
  print is
  (as, zs) <- MWC.create >>= runReaderT test
  let vs = Prelude.take bigT ts
  let us = Prelude.map (/ fromIntegral bigN) $
           Prelude.map (\i -> M.sum $ (transposeOuter zs) !> i !> 0) $
           Prelude.take bigT [0..]
  let bs = Prelude.map (\i -> as !> i !> 0) $
           Prelude.take bigT [0..]
  let cs = Prelude.map (\i -> as !> i !> 2) $
           Prelude.take bigT [0..]
  R.runRegion $ do
    _ <- [r| print(file.path(R.home("bin"), "R")) |]
    _ <- [r| library(ggplot2) |]
    df <- [r| data <- data.frame(vs_hs, us_hs, bs_hs) |]

    p1 <- [r| ggplot(df_hs, aes(x=vs_hs)) |]
    p2 <- [r| p1_hs + geom_line(aes(y = us_hs), color = "darkred") |]
    p3 <- [r| p2_hs + geom_line(aes(y = bs_hs), color="steelblue") |]
    _  <- [r| p3_hs + geom_point(aes(y = cs_hs), color = "red") |]
    _  <- [r| ggsave(filename="diagrams/viaR.png") |]
    return ()
  return ()
