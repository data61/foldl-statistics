# foldl-statistics
A reimplementation of the [Statistics.Sample](https://hackage.haskell.org/package/statistics/docs/Statistics-Sample.html)
Haskell module using the [foldl](https://www.stackage.org/lts-5.1/package/foldl) package.
The intention of this package is to allow these useful algorithms to be used on a much broader set of data input types,
including lists and streaming libraries such as `conduit` and `pipes`, and any other type which is `Foldable`.
All statistics in this package can be computed with no more than two passes over the data - once to compute the mean and once to compute 
any statistics which require the mean. this is achieved because foldl `Fold`s are `Applicative`, which means that to compute for example the first 4 central moments, as well as the count, the following could be used:

```haskell
import Control.Foldl as F

...

dataseries :: [Double]
dataseries = ...

    ...
    let m = F.fold mean dataseries
       (c2,c3,c4,c5,n) = flip F.fold dataseries $ 
                        (\(c2,c3) (c4,c5) n -> (c2,c3,c4,c5,n)) 
                        <$> centralMoment 2 3 m
                        <*> centralMoment 4 5 m
                        <*> F.length
```

which traverses the data twice, once to compute the mean, and once to compute all the central moments and the count concurrently. This brings along with it for free the ability to compute streaming statistics, such as the mean of all data seen so far, using the `foldl`'s `scan` function.

Where possible, care has been taken to ensure the numerical stability of the computation of statistics.

Several algorithms require the mean of the data to be known before computing the statistic, such as `skewness`, `kutosis` and other `centralMoment`s.
There are 'fast' implementations for calculating the variance, unbiased variance and standard deviation, which can be computed without knowing the mean
*a priori*, but which may produce less accurate results.

