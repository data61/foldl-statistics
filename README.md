# foldl-statistics
A reimplementation of the [Statistics.Sample](https://hackage.haskell.org/package/statistics/docs/Statistics-Sample.html)
Haskell module using the [foldl](https://www.stackage.org/lts-5.1/package/foldl) package.
The intention of this package is to allow these useful algorithms to be used on a much broader set of data input types,
including lists and streaming libraries such as `conduit` and `pipes`.

Where possible, care has been taken to ensure the numerical stability of the computation of statistics.

Several algorithms require the mean of the data to be known before computing the statistic, such as `skewness`, `kutosis` and other `centralMoment`s.
There are 'fast' implementations for calculating the variance, unbiased variance and standard deviation, which can be computed without knowing the mean
*a priori*, but which produce less accurate results.

