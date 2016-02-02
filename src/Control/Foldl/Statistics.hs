-- Copyright 2016 National ICT Australia

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Control.Foldl.Statistics (
    -- * Introduction
    -- $intro
    -- * Descriptive functions
    range
    , sum'

    -- * Statistics of location
    , mean
    , welfordMean
    , meanWeighted
    , harmonicMean
    , geometricMean

    -- * Statistics of dispersion
    -- $variance

    -- ** Functions over central moments
    , centralMoment
    , centralMoments
    , skewness
    , kurtosis

    -- ** Functions requiring the mean to be known (numerically robust)
    -- $robust
    , variance
    , varianceUnbiased
    , stdDev
    , varianceWeighted

    -- ** Single-pass functions (faster, less safe)
    -- $cancellation
    , fastVariance
    , fastVarianceUnbiased
    , fastStdDev

    -- * References
    -- $references
    , module F

    ) where

import Control.Foldl as F
import Data.Profunctor

import Numeric.Sum

data T  = T  {-# UNPACK #-}!Double {-# UNPACK #-}!Int
data T1 = T1 {-# UNPACK #-}!Int    {-# UNPACK #-}!Double {-# UNPACK #-}!Double
data V  = V  {-# UNPACK #-}!Double {-# UNPACK #-}!Double
data V1 = V1 {-# UNPACK #-}!Double {-# UNPACK #-}!Double {-# UNPACK #-}!Int
data V1S = V1S {-# UNPACK #-}!KBNSum {-# UNPACK #-}!KBNSum {-# UNPACK #-}!Int


-- $intro
-- Statistical functions taken from the "Statistics.Sample" module of the
-- <https://hackage.haskell.org/package/statistics statistics> package by
-- Brian O'Sullivan, implemented as `Control.Foldl.Fold's from the
-- <https://hackage.haskell.org/package/foldl foldl> package.
--
-- This allows many statistics to be computed concurrently with at most
-- two passes over the data, usually by computing the `mean' first, and
-- passing it to further `Fold's.

-- | A numerically stable sum using Kahan-Babuška-Neumaier
-- summation from "Numeric.Sum"
{-# INLINE sum' #-}
sum' :: Fold Double Double
sum' = Fold (add :: KBNSum -> Double -> KBNSum)
            (zero :: KBNSum)
            kbn


-- | Range. The difference between the largest and smallest
-- elements of a sample.
{-# INLINE range #-}
range :: Fold Double Double
range = (\(Just lo) (Just hi) -> hi - lo)
        <$> F.minimum
        <*> F.maximum

-- | Arithmetic mean.  This uses Kahan-Babuška-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large.
{-# INLINE mean #-}
mean :: Fold Double Double
mean = (\s n -> s / fromIntegral (n::Int))
        <$> sum'
        <*> F.length

-- | Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
--
-- Compared to 'mean', this loses a surprising amount of precision
-- unless the inputs are very large.
{-# INLINE welfordMean #-}
welfordMean :: Fold Double Double
welfordMean = Fold step (T 0 0) final where
    final (T m _) = m
    step (T m n) x = T m' n' where
        m' = m + (x - m) / fromIntegral n'
        n' = n + 1


-- | Arithmetic mean for weighted sample. It uses a single-pass
-- algorithm analogous to the one used by 'welfordMean'.
{-# INLINE meanWeighted #-}
meanWeighted :: Fold (Double,Double) Double
meanWeighted = Fold step (V 0 0) final
    where
      final (V a _) = a
      step (V m w) (x,xw) = V m' w'
          where m' | w' == 0   = 0
                   | otherwise = m + xw * (x - m) / w'
                w' = w + xw

-- | Harmonic mean.  This algorithm performs a single pass over
-- the sample.
{-# INLINE harmonicMean #-}
harmonicMean :: Fold Double Double
harmonicMean = Fold step (T 0 0) final
  where
    final (T b a) = fromIntegral a / b
    step (T x y) n = T (x + (1/n)) (y+1)

-- | Geometric mean of a sample containing no negative values.
{-# INLINE geometricMean #-}
geometricMean :: Fold Double Double
geometricMean = dimap log exp mean

-- | Compute the /k/th central moment of a sample.  The central moment
-- is also known as the moment about the mean.
--
-- This function requires the mean of the data to compute the central moment.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
{-# INLINE centralMoment #-}
centralMoment :: Int -> Double -> Fold Double Double
centralMoment a m
    | a < 0  = error "Statistics.Sample.centralMoment: negative input"
    | a == 0 = 1
    | a == 1 = 0
    | otherwise = lmap go sum' / (fromIntegral <$> F.length)
  where
    go x = (x-m) ^^^ a

-- | Compute the /k/th and /j/th central moments of a sample.
--
-- This fold requires the mean of the data to be known.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
{-# INLINE centralMoments #-}
centralMoments :: Int -> Int -> Double -> Fold Double (Double, Double)
centralMoments a b m
    | a < 2 || b < 2 = (,) <$> centralMoment a m <*> centralMoment b m
    | otherwise      = Fold step (V1 0 0 0) final
  where step (V1 i j n) x = V1 (i + d^^^a) (j + d^^^b) (n+1)
            where d  = x - m
        final (V1 i j n) = (i / fromIntegral n , j / fromIntegral n)


-- | Compute the /k/th and /j/th central moments of a sample.
--
-- This fold requires the mean of the data to be known.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
{-# INLINE centralMoments' #-}
centralMoments' :: Int -> Int -> Double -> Fold Double (Double, Double)
centralMoments' a b m
    | a < 2 || b < 2 = (,) <$> centralMoment a m <*> centralMoment b m
    | otherwise      = Fold step (V1S zero zero 0) final
  where step (V1S i j n) x = V1S (add i $ d^^^a) (add j $ d^^^b) (n+1)
            where d  = x - m
        final (V1S i j n) = (kbn i / fromIntegral n , kbn j / fromIntegral n)

-- | Compute the skewness of a sample. This is a measure of the
-- asymmetry of its distribution.
--
-- A sample with negative skew is said to be /left-skewed/.  Most of
-- its mass is on the right of the distribution, with the tail on the
-- left.
--
-- > skewness $ U.to [1,100,101,102,103]
-- > ==> -1.497681449918257
--
-- A sample with positive skew is said to be /right-skewed/.
--
-- > skewness $ U.to [1,2,3,4,100]
-- > ==> 1.4975367033335198
--
-- A sample's skewness is not defined if its 'variance' is zero.
--
-- This fold requires the mean of the data to be known.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
{-# INLINE skewness #-}
skewness :: Double -> Fold Double Double
skewness m = (\(c3, c2) -> c3 * c2 ** (-1.5)) <$> centralMoments 3 2 m


-- | Compute the excess kurtosis of a sample.  This is a measure of
-- the \"peakedness\" of its distribution.  A high kurtosis indicates
-- that more of the sample's variance is due to infrequent severe
-- deviations, rather than more frequent modest deviations.
--
-- A sample's excess kurtosis is not defined if its 'variance' is
-- zero.
--
-- This fold requires the mean of the data to be known.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
{-# INLINE kurtosis #-}
kurtosis :: Double -> Fold Double Double
kurtosis m = (\(c4,c2) -> c4 / (c2 * c2) - 3) <$> centralMoments 4 2 m


-- $variance
--
-- The variance&#8212;and hence the standard deviation&#8212;of a
-- sample of fewer than two elements are both defined to be zero.

-- $robust
--
-- These functions use the compensated summation algorithm of Chan et
-- al. for numerical robustness, but require two passes over the
-- sample data as a result.
--
-- Because of the need for two passes, these functions are /not/
-- subject to stream fusion.


-- Multiply a number by itself.
{-# INLINE square #-}
square :: Double -> Double
square x = x * x

{-# INLINE robustSumVar #-}
robustSumVar :: Double -> Fold Double Double
robustSumVar m = lmap (square . subtract m) sum'

-- | Maximum likelihood estimate of a sample's variance.  Also known
-- as the population variance, where the denominator is /n/.
{-# INLINE variance #-}
variance :: Double -> Fold Double Double
variance m =
    (\sv n -> if n > 1 then sv / fromIntegral n else 0)
    <$> robustSumVar m
    <*> F.length

-- | Unbiased estimate of a sample's variance.  Also known as the
-- sample variance, where the denominator is /n/-1.
{-# INLINE varianceUnbiased #-}
varianceUnbiased :: Double -> Fold Double Double
varianceUnbiased m =
    (\sv n -> if n > 1 then sv / fromIntegral (n-1) else 0)
    <$> robustSumVar m
    <*> F.length


-- | Standard deviation.  This is simply the square root of the
-- unbiased estimate of the variance.
{-# INLINE stdDev #-}
stdDev :: Double -> Fold Double Double
stdDev m = sqrt (varianceUnbiased m)


{-# INLINE robustSumVarWeighted #-}
robustSumVarWeighted :: Double -> Fold (Double,Double) V1
robustSumVarWeighted m = Fold step (V1 0 0 0) id
    where
      step (V1 s w n) (x,xw) = V1 (s + xw*d*d) (w + xw) (n+1)
          where d = x - m

-- | Weighted variance. This is biased estimation. Requires the
-- weighted mean of the input data.
{-# INLINE varianceWeighted #-}
varianceWeighted :: Double -> Fold (Double,Double)  Double
varianceWeighted m =
    (\(V1 s w n) -> if n > 1 then s / w else 0)
    <$> robustSumVarWeighted m

-- $cancellation
--
-- The functions prefixed with the name @fast@ below perform a single
-- pass over the sample data using Knuth's algorithm. They usually
-- work well, but see below for caveats. These functions are subject
-- to fusion and do not require the mean to be passed.
--
-- /Note/: in cases where most sample data is close to the sample's
-- mean, Knuth's algorithm gives inaccurate results due to
-- catastrophic cancellation.

{-# INLINE fastVar #-}
fastVar :: Fold Double T1
fastVar = Fold step (T1 0 0 0) id
  where
    step (T1 n m s) x = T1 n' m' s'
      where n' = n + 1
            m' = m + d / fromIntegral n'
            s' = s + d * (x - m')
            d  = x - m

-- | Maximum likelihood estimate of a sample's variance.
{-# INLINE fastVariance #-}
fastVariance :: Fold Double Double
fastVariance = final <$> fastVar
  where final (T1 n _m s)
          | n > 1     = s / fromIntegral n
          | otherwise = 0


-- | Maximum likelihood estimate of a sample's variance.
{-# INLINE fastVarianceUnbiased #-}
fastVarianceUnbiased :: Fold Double Double
fastVarianceUnbiased = final <$> fastVar
  where final (T1 n _m s)
          | n > 1     = s / fromIntegral (n-1)
          | otherwise = 0


-- | Standard deviation.  This is simply the square root of the
-- maximum likelihood estimate of the variance.
{-# INLINE fastStdDev #-}
fastStdDev :: Fold Double Double
fastStdDev = sqrt fastVariance


-- $references
--
-- * Chan, T. F.; Golub, G.H.; LeVeque, R.J. (1979) Updating formulae
--   and a pairwise algorithm for computing sample
--   variances. Technical Report STAN-CS-79-773, Department of
--   Computer Science, Stanford
--   University. <ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf>
--
-- * Knuth, D.E. (1998) The art of computer programming, volume 2:
--   seminumerical algorithms, 3rd ed., p. 232.
--
-- * Welford, B.P. (1962) Note on a method for calculating corrected
--   sums of squares and products. /Technometrics/
--   4(3):419&#8211;420. <http://www.jstor.org/stable/1266577>
--
-- * West, D.H.D. (1979) Updating mean and variance estimates: an
--   improved method. /Communications of the ACM/
--   22(9):532&#8211;535. <http://doi.acm.org/10.1145/359146.359153>



-- (^) operator from Prelude is just slow.
(^^^) :: Double -> Int -> Double
x ^^^ 1 = x
x ^^^ n = x * (x ^^^ (n-1))
{-# INLINE (^^^) #-}