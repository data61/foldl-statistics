-- |
-- Module    : Control.Foldl.Statistics
-- Copyright : (c) 2011 Bryan O'Sullivan, 2016 National ICT Australia
-- License   : BSD3
--
-- Maintainer  : alex.mason@nicta.com.au
-- Stability   : experimental
-- Portability : portable
--

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
    , centralMoments'
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
    , fastLMVSK
    , LMVSK(..)
    , LMVSKState
    , foldLMVSKState
    , getLMVSK
    , fastLinearReg
    , LinRegResult(..)


    , correlation

    -- * References
    -- $references
    , module Control.Foldl

    ) where

import Control.Foldl as F
import qualified Control.Foldl
import Data.Profunctor
import Data.Semigroup

import Numeric.Sum (KBNSum, kbn, add, zero)

data T   = T   {-# UNPACK #-}!Double {-# UNPACK #-}!Int
data TS  = TS  {-# UNPACK #-}!KBNSum {-# UNPACK #-}!Int
data T1  = T1  {-# UNPACK #-}!Int    {-# UNPACK #-}!Double {-# UNPACK #-}!Double
data V   = V   {-# UNPACK #-}!Double {-# UNPACK #-}!Double
data V1  = V1  {-# UNPACK #-}!Double {-# UNPACK #-}!Double {-# UNPACK #-}!Int
data V1S = V1S {-# UNPACK #-}!KBNSum {-# UNPACK #-}!KBNSum {-# UNPACK #-}!Int


-- $intro
-- Statistical functions from the
-- <https://hackage.haskell.org/package/statistics/docs/Statistics-Sample.html Statistics.Sample>
-- module of the
-- <https://hackage.haskell.org/package/statistics statistics> package by
-- Bryan O'Sullivan, implemented as `Control.Foldl.Fold's from the
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


-- | The difference between the largest and smallest
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
mean = Fold step (TS zero 0) final where
    step  (TS s n) x = TS (add s x) (n+1)
    final (TS s n)   = kbn s / fromIntegral n


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

-- | Harmonic mean.
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
    | otherwise = Fold step (TS zero 0) final where
        step  (TS s n) x = TS (add s $ go x) (n+1)
        final (TS s n)   = kbn s / fromIntegral n
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
  where final (V1 i j n)   = (i / fromIntegral n , j / fromIntegral n)
        step  (V1 i j n) x = V1 (i + d^^^a) (j + d^^^b) (n+1)
            where d  = x - m


-- | Compute the /k/th and /j/th central moments of a sample.
--
-- This fold requires the mean of the data to be known.
--
-- This variation of `centralMoments' uses Kahan-Babuška-Neumaier
-- summation to attempt to improve the accuracy of results, which may
-- make computation slower.
{-# INLINE centralMoments' #-}
centralMoments' :: Int -> Int -> Double -> Fold Double (Double, Double)
centralMoments' a b m
    | a < 2 || b < 2 = (,) <$> centralMoment a m <*> centralMoment b m
    | otherwise      = Fold step (V1S zero zero 0) final
  where final (V1S i j n)   = (kbn i / fromIntegral n , kbn j / fromIntegral n)
        step  (V1S i j n) x = V1S (add i $ d^^^a) (add j $ d^^^b) (n+1)
            where d  = x - m

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
--
-- Many of these Folds take the mean as an argument for constructing
-- the variance, and as such require two passes over the data.

-- $robust
--
-- These functions use the compensated summation algorithm of Chan et
-- al. for numerical robustness, but require two passes over the
-- sample data as a result.


-- Multiply a number by itself.
{-# INLINE square #-}
square :: Double -> Double
square x = x * x

{-# INLINE robustSumVar #-}
robustSumVar :: Double -> Fold Double TS
robustSumVar m = Fold step (TS zero 0) id where
    step  (TS s n) x = TS (add s . square . subtract m $ x) (n+1)

-- | Maximum likelihood estimate of a sample's variance.  Also known
-- as the population variance, where the denominator is /n/.
{-# INLINE variance #-}
variance :: Double -> Fold Double Double
variance m =
    (\(TS sv n) -> if n > 1 then kbn sv / fromIntegral n else 0)
    <$> robustSumVar m

-- | Unbiased estimate of a sample's variance.  Also known as the
-- sample variance, where the denominator is /n/-1.
{-# INLINE varianceUnbiased #-}
varianceUnbiased :: Double -> Fold Double Double
varianceUnbiased m =
    (\(TS sv n) -> if n > 1 then kbn sv / fromIntegral (n-1) else 0)
    <$> robustSumVar m


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



-- | When returned by `fastLMVSK`, contains the count, mean,
--  variance, skewness and kurtosis of a series of samples.
--
-- /Since: 0.1.1.0/
data LMVSK  = LMVSK
  { lmvskCount    :: {-# UNPACK #-}!Int
  , lmvskMean     :: {-# UNPACK #-}!Double
  , lmvskVariance :: {-# UNPACK #-}!Double
  , lmvskSkewness :: {-# UNPACK #-}!Double
  , lmvskKurtosis :: {-# UNPACK #-}!Double
  } deriving (Show, Eq)

newtype LMVSKState = LMVSKState LMVSK

instance Monoid LMVSKState where
  {-# INLINE mempty #-}
  mempty = LMVSKState lmvsk0
  {-# INLINE mappend #-}
  mappend = (<>)

instance Semigroup LMVSKState where
  {-# INLINE (<>) #-}
  (LMVSKState (LMVSK an am1 am2 am3 am4)) <> (LMVSKState (LMVSK bn bm1 bm2 bm3 bm4))
    = LMVSKState (LMVSK n m1 m2 m3 m4) where
    fi :: Int -> Double
    fi = fromIntegral
    -- combined.n = a.n + b.n;
    n      = an+bn
    n2     = n*n
    nd     = fi n
    and    = fi an
    bnd    = fi bn
    -- delta = b.M1 - a.M1;
    delta  =    bm1 - am1
    -- delta2 = delta*delta;
    delta2 =    delta*delta
    -- delta3 = delta*delta2;
    delta3 =    delta*delta2
    -- delta4 = delta2*delta2;
    delta4 =    delta2*delta2
    -- combined.M1 = (a.n*a.M1 + b.n*b.M1) / combined.n;
    m1     =         (and*am1  + bnd*bm1 ) / nd
    -- combined.M2 = a.M2 + b.M2 + delta2*a.n*b.n / combined.n;
    m2     =          am2 + bm2  + delta2*and*bnd / nd
    -- combined.M3 = a.M3 + b.M3 + delta3*a.n*b.n*   (a.n - b.n)/(combined.n*combined.n);
    m3     =         am3  + bm3  + delta3*and*bnd* fi( an - bn )/ fi n2
    -- combined.M3 += 3.0*delta * (a.n*b.M2 - b.n*a.M2) / combined.n;
           +          3.0*delta * (and*bm2  - bnd*am2 ) / nd
    --
    -- combined.M4 = a.M4 + b.M4 + delta4*a.n*b.n * (a.n*a.n - a.n*b.n + b.n*b.n) /(combined.n*combined.n*combined.n);
    m4     =         am4  + bm4  + delta4*and*bnd *fi(an*an  -  an*bn  +  bn*bn ) / fi (n*n*n)
    -- combined.M4 += 6.0*delta2 * (a.n*a.n*b.M2 + b.n*b.n*a.M2)/(combined.n*combined.n) +
           +          6.0*delta2 * (and*and*bm2  + bnd*bnd*am2) / fi n2
    --               4.0*delta*(a.n*b.M3 - b.n*a.M3) / combined.n;
           +         4.0*delta*(and*bm3  - bnd*am3) / nd

-- | Efficiently compute the
-- __length, mean, variance, skewness and kurtosis__ with a single pass.
--
-- /Since: 0.1.1.0/
{-# INLINE fastLMVSK #-}
fastLMVSK :: Fold Double LMVSK
fastLMVSK = getLMVSK <$> foldLMVSKState


{-# INLINE lmvsk0 #-}
lmvsk0 = LMVSK 0 0 0 0 0

-- | Performs the heavy lifting of fastLMVSK. This is exposed
--   because the internal `LMVSKState` is monoidal, allowing you
--   to run these statistics in parallel over datasets which are
--   split and then combine the results.
--
-- /Since: 0.1.2.0/
{-# INLINE foldLMVSKState #-}
foldLMVSKState :: Fold Double LMVSKState
foldLMVSKState = Fold stepLMVSKState (LMVSKState lmvsk0) id

{-# INLINE stepLMVSKState #-}
stepLMVSKState :: LMVSKState -> Double -> LMVSKState
stepLMVSKState (LMVSKState (LMVSK n1 m1 m2 m3 m4)) x = LMVSKState $ LMVSK n m1' m2' m3' m4' where
  fi :: Int -> Double
  fi = fromIntegral
  -- long long n1 = n;
  -- n++;
  n = n1+1
  -- delta = x - M1;
  delta =    x - m1
  -- delta_n = delta / n;
  delta_n =    delta / fi n
  -- delta_n2 = delta_n * delta_n;
  delta_n2 =    delta_n * delta_n
  -- term1 = delta * delta_n * n1;
  term1 =    delta * delta_n * fi n1
  -- M1 +=   delta_n;
  m1' = m1 + delta_n
  -- M4 +=   term1 * delta_n2 *    (n*n - 3*n + 3) + 6 * delta_n2 * M2 - 4 * delta_n * M3;
  m4' = m4 + term1 * delta_n2 * fi (n*n - 3*n + 3) + 6 * delta_n2 * m2 - 4 * delta_n * m3
  -- M3 +=   term1 * delta_n *    (n - 2) - 3 * delta_n * M2;
  m3' = m3 + term1 * delta_n * fi (n - 2) - 3 * delta_n * m2
  -- M2 +=  term1;
  m2' = m2 + term1

-- | Returns the stats which have been computed in a LMVSKState.
--
-- /Since: 0.1.2.0/
getLMVSK :: LMVSKState -> LMVSK
getLMVSK (LMVSKState (LMVSK n m1 m2 m3 m4)) = LMVSK n m1 m2' m3' m4' where
  nd = fromIntegral n
  -- M2/(n-1.0)
  m2' = m2 / (nd-1)
  --    sqrt(double(n)) * M3/ pow(M2, 1.5)
  m3' = sqrt nd * m3 / (m2 ** 1.5)
  -- double(n)*M4 / (M2*M2) - 3.0
  m4' = nd*m4     / (m2*m2) - 3.0



-- | When returned by `fastLinearReg`, contains the count,
--   slope, intercept and correlation of combining @(x,y)@ pairs.
--
-- /Since: 0.1.1.0/
data LinRegResult = LinRegResult
  {lrrCount       :: {-# UNPACK #-}!Int
  ,lrrSlope       :: {-# UNPACK #-}!Double
  ,lrrIntercept   :: {-# UNPACK #-}!Double
  ,lrrCorrelation :: {-# UNPACK #-}!Double
  } deriving (Show, Eq)

-- | Computes the __count, slope, (Y) intercept and correlation__ of @(x,y)@
--   pairs.
--
-- >>> F.fold fastLinearReg $ map (\x -> (x,3*x+7)) [1..100]
-- LinRegResult {lrrCount = 100, lrrSlope = 3.0,
--               lrrIntercept = 7.0, lrrCorrelation = 1.0}
--
-- >>> F.fold fastLinearReg $ map (\x -> (x,0.005*x*x+3*x+7)) [1..100]
-- LinRegResult {
--    lrrCount = 100,
--    lrrSlope = 3.5049999999999994,
--    lrrIntercept = -1.5849999999999795,
--    lrrCorrelation = 0.9993226275740273}
--
-- /Since: 0.1.1.0/
{-# INLINE fastLinearReg #-}
fastLinearReg :: Fold (Double,Double) LinRegResult
fastLinearReg = Fold step (V2 0 (V 0 0) (V 0 0) 0) final where
  step (V2 n v1@(V xMean xVar) v2@(V yMean _) s_xy) (x,y) = V2 (n+1) v1' v2' s_xy' where
    nd = fromIntegral n
    nd1 = fromIntegral (n+1)
    s_xy' = s_xy + (xMean - x)*(yMean - y)*nd/nd1
    v1' = stepV v1 n x
    v2' = stepV v2 n y
  final (V2 n v1@(V xMean xVar) v2@(V yMean yVar)  s_xy) = LinRegResult n slope intercept correlation where
    ndm1 = fromIntegral (n-1)
    slope = s_xy / xVar
    intercept = yMean - slope*xMean
    t = sqrt (xVar/ndm1) * sqrt (yVar/ndm1); -- stddev x * stddev y
    correlation = s_xy / (ndm1 * t)

data V2 = V2 {-# UNPACK #-}!Int {-# UNPACK #-}!V {-# UNPACK #-}!V {-# UNPACK #-}!Double

{-# INLINE stepV #-}
stepV :: V -> Int -> Double -> V
stepV (V m1 m2) n1 x = V m1' m2' where
  delta = x - m1
  delta_n = delta / fromIntegral (n1+1)
  term1 = delta * delta_n * fromIntegral n1
  m1' = m1 + delta_n
  m2' = m2 + term1



-- | Given the mean and standard deviation of two distributions, computes
--   the correlation between them.
correlation :: (Double, Double) -> (Double, Double) -> Fold (Double,Double) Double
correlation (m1,m2) (s1,s2) = Fold step (TS zero 0) final where
    step  (TS s n) (x1,x2) = TS (add s $ ((x1-m1)/s1) * ((x2-m2)/s2)) (n+1)
    final (TS s n)         = kbn s / fromIntegral (n-1)


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
--
-- * John D. Cook. Computing skewness and kurtosis in one pass
--   <http://www.johndcook.com/blog/skewness_kurtosis/>



-- (^) operator from Prelude is just slow.
(^^^) :: Double -> Int -> Double
x ^^^ 1 = x
x ^^^ n = x * (x ^^^ (n-1))
{-# INLINE[2] (^^^) #-}
{-# RULES
"pow 2"  forall x. x ^^^ 2  = x * x
"pow 3"  forall x. x ^^^ 3  = x * x * x
"pow 4"  forall x. x ^^^ 4  = x * x * x * x
"pow 5"  forall x. x ^^^ 5  = x * x * x * x * x
"pow 6"  forall x. x ^^^ 6  = x * x * x * x * x * x
"pow 7"  forall x. x ^^^ 7  = x * x * x * x * x * x * x
"pow 8"  forall x. x ^^^ 8  = x * x * x * x * x * x * x * x
"pow 9"  forall x. x ^^^ 9  = x * x * x * x * x * x * x * x * x
"pow 10" forall x. x ^^^ 10 = x * x * x * x * x * x * x * x * x * x

 #-}
