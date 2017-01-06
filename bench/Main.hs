{-# LANGUAGE CPP #-}

module Main where

import           Control.Monad.ST         (runST)
import           Criterion.Main
import qualified Data.Vector.Unboxed      as U
import qualified Statistics.Sample        as S
import           Statistics.Transform
import           System.Random.MWC
#if MIN_VERSION_foldl(1,2,2)
import           Control.Foldl            as F hiding (mean, variance)
#else
import           Control.Foldl            as F
#endif

import           Control.Foldl.Statistics


-- Test sample
{-# NOINLINE sample #-}
sample :: U.Vector Double
sample = runST $ flip uniformVector 10000 =<< create

{-# NOINLINE sample2 #-}
sample2 :: U.Vector (Double,Double)
sample2 = runST $ flip uniformVector 10000 =<< create

{-# NOINLINE absSample #-}
absSample = U.map abs sample

-- Weighted test sample
{-# NOINLINE sampleW #-}
sampleW :: U.Vector (Double,Double)
sampleW = U.zip sample (U.reverse sample)

m = F.fold mean (U.toList sample)

mw = F.fold meanWeighted (U.toList sampleW)




main :: IO ()
main = defaultMain
    [ bgroup "Statistics of location"
      [ bgroup "mean"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold mean (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.mean sample
        ]
      , bgroup "meanWeighted"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold meanWeighted (U.toList vec)) sampleW
        , bench "Statistics.Sample"  $ nf S.meanWeighted sampleW
        ]
      , bgroup "welfordMean"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold welfordMean (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.welfordMean sample
        ]
      , bgroup "harmonicMean"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold harmonicMean (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.harmonicMean sample
        ]
      , bgroup "geometricMean"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold geometricMean (U.toList vec)) absSample
        , bench "Statistics.Sample"  $ nf S.geometricMean absSample
        ]
      ]
    , bgroup "Single-pass"
      [ bgroup "fastVariance"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold fastVariance (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.fastVariance sample
        ]
      , bgroup "fastVarianceUnbiased"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold fastVarianceUnbiased (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.fastVarianceUnbiased sample
        ]
      , bgroup "fastStdDev"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold fastStdDev (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.fastStdDev sample
        ]
      , bgroup "fastLMVSK"
                       -- T4 is strict in all arguments, so WHNF ok here
          [bench "C.F.Statistics"      $ whnf (\vec -> F.fold fastLMVSK (U.toList vec)) sample
          ]
      , bgroup "fastLinearReg"
          [bench "fastLinearReg"       $ whnf (\vec -> F.fold fastLinearReg (U.toList vec)) sample2
          ]
      ]

    , bgroup "requiring the mean"
      [ bgroup "variance"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (variance m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (variance (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.variance sample
        ]
      , bgroup "varianceUnbiased"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (varianceUnbiased m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (varianceUnbiased (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.varianceUnbiased sample
        ]
      , bgroup "stdDev"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (stdDev m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (stdDev (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.stdDev sample
        ]
      , bgroup "varianceWeighted"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (varianceWeighted m) (U.toList vec)) sampleW
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (varianceWeighted (F.fold meanWeighted (U.toList vec))) (U.toList vec)) sampleW
        , bench "Statistics.Sample"  $ nf S.varianceWeighted sampleW
        ]
      ]

    , bgroup "over central moments"
      [ bgroup "skewness"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (skewness m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (skewness (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.skewness sample
        ]
      , bgroup "kurtosis"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (kurtosis m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (kurtosis (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf S.kurtosis sample
        ]
      , bgroup "centralMoment 2"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (centralMoment 2 m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (centralMoment 2 (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf (S.centralMoment 2) sample
        ]
      , bgroup "centralMoment 3"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (centralMoment 3 m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (centralMoment 3 (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf (S.centralMoment 3) sample
        ]
      , bgroup "centralMoment 4"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (centralMoment 4 m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (centralMoment 4 (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf (S.centralMoment 4) sample
        ]
      , bgroup "centralMoment 7"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (centralMoment 7 m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (centralMoment 7 (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf (S.centralMoment 7) sample
        ]
      , bgroup "centralMoments 4 9"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (centralMoments 4 9 m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (centralMoments 4 9 (F.fold mean (U.toList vec))) (U.toList vec)) sample
        , bench "Statistics.Sample"  $ nf (S.centralMoments 4 9) sample
        ]
      , bgroup "centralMoments' 4 9"
        [ bench "C.F.Statistics"     $ nf (\vec -> F.fold (centralMoments' 4 9 m) (U.toList vec)) sample
        , bench "C.F.S(comp mean)"   $ nf (\vec -> F.fold (centralMoments' 4 9 (F.fold mean (U.toList vec))) (U.toList vec)) sample
        ]
      ]
    ]
