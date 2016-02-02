
import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC

import qualified Control.Foldl as F
import Control.Foldl.Statistics hiding (length)

import qualified Data.Vector.Unboxed as U
import Test.QuickCheck.Instances

import qualified Statistics.Sample as S


toV :: [Double] -> U.Vector Double
toV = U.fromList

main :: IO ()
main = defaultMain $
    testGroup "Match Statistics.Sample"
        [ testGroup "Without pre-computed mean"
            [ testGroup "Statistics of location"
                [ QC.testProperty "mean" $ \lst -> not (Prelude.null lst) ==>
                    F.fold mean lst == S.mean (toV lst)
                , QC.testProperty "meanWeighted" $ \lst -> not (Prelude.null lst) ==>
                    F.fold meanWeighted lst == S.meanWeighted (U.fromList lst)
                , QC.testProperty "welfordMean" $ \lst -> not (Prelude.null lst) ==>
                    F.fold welfordMean lst == S.welfordMean (toV lst)
                , QC.testProperty "harmonicMean" $ \lst -> not (Prelude.null lst) ==>
                    F.fold harmonicMean lst == S.harmonicMean (toV lst)
                , QC.testProperty "geometricMean" $ \lst -> not (Prelude.null lst) ==>
                    let lst' = map abs lst
                    in F.fold geometricMean lst' == S.geometricMean (toV lst')
                ]

            , testGroup "Single-pass functions"
                [ QC.testProperty "fastVariance" $ \lst -> not (Prelude.null lst) ==>
                    F.fold fastVariance lst == S.fastVariance (toV lst)
                , QC.testProperty "fastVarianceUnbiased" $ \lst -> not (Prelude.null lst) ==>
                    F.fold fastVarianceUnbiased lst == S.fastVarianceUnbiased (toV lst)
                , QC.testProperty "fastStdDev" $ \lst -> not (Prelude.null lst) ==>
                    F.fold fastStdDev lst == S.fastStdDev (toV lst)
                ]
            ]

        , testGroup "With pre-computed mean"
            [ testGroup "Functions requiring the mean to be known"
                [ QC.testProperty "variance" $ \lst -> not (Prelude.null lst) ==>
                    let m = F.fold mean lst
                    in F.fold (variance m) lst == S.variance (toV lst)
                , QC.testProperty "varianceUnbiased" $ \lst -> not (Prelude.null lst) ==>
                    let m = F.fold mean lst
                    in F.fold (varianceUnbiased m) lst == S.varianceUnbiased (toV lst)
                , QC.testProperty "stdDev" $ \lst -> not (Prelude.null lst) ==>
                    let m = F.fold mean lst
                    in F.fold (stdDev m) lst == S.stdDev (toV lst)
                , QC.testProperty "varianceWeighted" $ \lst -> not (Prelude.null lst) ==>
                    let m = F.fold meanWeighted lst
                    in F.fold (varianceWeighted m) lst == S.varianceWeighted (U.fromList lst)
                ]

            , testGroup "Functions over central moments"
                [ QC.testProperty "skewness" $ \lst -> length lst > 3 ==>
                    let m = F.fold mean lst
                    in F.fold (skewness m) lst == S.skewness (toV lst)
                , QC.testProperty "kurtosis" $ \lst -> length lst > 4 ==>
                    let m = F.fold mean lst
                    in F.fold (kurtosis m) lst == S.kurtosis (toV lst)
                , QC.testProperty "centralMoment 2" $ \lst ->  length lst > 2 ==>
                    let m = F.fold mean lst
                    in F.fold (centralMoment 2 m) lst == S.centralMoment 2 (toV lst)
                , QC.testProperty "centralMoment 3" $ \lst -> length lst > 3 ==>
                    let m = F.fold mean lst
                    in F.fold (centralMoment 3 m) lst == S.centralMoment 3 (toV lst)
                , QC.testProperty "centralMoment 4" $ \lst -> length lst > 4 ==>
                    let m = F.fold mean lst
                    in F.fold (centralMoment 4 m) lst == S.centralMoment 4 (toV lst)
                , QC.testProperty "centralMoment 7" $ \lst -> length lst > 7 ==>
                    let m = F.fold mean lst
                    in F.fold (centralMoment 7 m) lst == S.centralMoment 7 (toV lst)
                ]
            ]
        ]
