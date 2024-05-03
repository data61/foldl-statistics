{-# LANGUAGE CPP #-}

import           Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import           Test.Tasty.QuickCheck     ((==>))
import qualified Test.Tasty.QuickCheck     as QC

#if MIN_VERSION_foldl(1,2,2)
import qualified Control.Foldl             as F hiding (mean, variance)
#else
import qualified Control.Foldl             as F
#endif

import           Control.Foldl.Statistics  hiding (length)

import qualified Data.Vector.Unboxed       as U
import           Test.QuickCheck.Instances ()

import qualified Statistics.Sample         as S

import           Data.Profunctor

import           Data.Function             (on)

import           Data.Semigroup            ((<>))
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
import           Data.Monoid               (mappend)
#endif



toV :: [Double] -> U.Vector Double
toV = U.fromList


onVec :: String -> (U.Vector Double -> QC.Property) -> TestTree
onVec str f = QC.testProperty str (f . toV)

onVec2 :: String -> (U.Vector (Double,Double) -> QC.Property) -> TestTree
onVec2 str f = QC.testProperty str (f . U.fromList)


testLMVSK :: Double -> Fold Double LMVSK
testLMVSK m = LMVSK
  <$> F.length
  <*> mean
  <*> variance m
  <*> skewness m
  <*> kurtosis m

precision :: Double
precision = 10e-9

cmpLMVSK :: Double -> LMVSK -> LMVSK -> Bool
cmpLMVSK prec a b = let
  t f = on (withinPCT prec) f a b
  in a == b ||
        (  t lmvskMean
        && t lmvskVariance
        && t lmvskKurtosis
        && t lmvskSkewness
        && ((==) `on` lmvskCount) a b
        )

diffLMVSK :: LMVSK -> LMVSK -> LMVSK
diffLMVSK a b = LMVSK
    (t lmvskCount)
    (t lmvskMean)
    (t lmvskVariance)
    (t lmvskSkewness)
    (t lmvskKurtosis)
    where t f = f a - f b


main :: IO ()
main = defaultMain $
    testGroup "Results match Statistics.Sample"
        [ testGroup "Without pre-computed mean"
            [ testGroup "Statistics of location"
                [ onVec "mean" $ \vec ->
                     not (U.null vec) ==> F.fold mean (U.toList vec) == S.mean vec
                , onVec2 "meanWeighted" $ \vec ->
                     not (U.null vec) ==> F.fold meanWeighted (U.toList vec) == S.meanWeighted vec
                , onVec "welfordMean" $ \vec ->
                     not (U.null vec) ==> F.fold welfordMean (U.toList vec) == S.welfordMean vec
                , onVec "harmonicMean" $ \vec ->
                     not (U.null vec) ==> F.fold harmonicMean (U.toList vec) == S.harmonicMean vec
                , onVec "geometricMean" $ \vec ->
                     not (U.null vec) ==>
                        let vec' = U.map abs vec
                            res = S.geometricMean vec'
                        in isNaN res || F.fold geometricMean (U.toList vec') == res
                ]

            , testGroup "Single-pass functions" $
                [ onVec "fastVariance" $ \vec ->
                    not (U.null vec) ==> F.fold fastVariance (U.toList vec) == S.fastVariance vec
                , onVec "fastVarianceUnbiased" $ \vec ->
                    not (U.null vec) ==> F.fold fastVarianceUnbiased (U.toList vec) == S.fastVarianceUnbiased vec
                , onVec "fastStdDev" $ \vec ->
                    not (U.null vec) ==> F.fold fastStdDev (U.toList vec) == S.fastStdDev vec
                , let
                  -- TODO: Known failure when using
                  -- --quickcheck-replay '39 TFGenR A6EB566E901D554AAA13826C088B8831192E813D893D082A85F8A27C86D569E0 0 65535 16 0'
                  in onVec ("fastLMVSK within " ++ show precision ++ " %") $ \vec ->
                    U.length vec > 2 ==> let
                      m         = F.fold mean $ U.toList vec
                      fast      = F.fold fastLMVSK $ U.toList vec
                      reference = F.fold (testLMVSK m) $ U.toList vec
                      in QC.counterexample (unlines ["",show fast,show reference, "Diff:", show (diffLMVSK fast reference)]) $
                            cmpLMVSK precision fast reference
                , QC.testProperty "LMVSKSemigroup" $ \v1 v2 ->
                    U.length v1 > 2 && U.length v2 > 2 && U.sum (mappend v1 v1) /= U.product (mappend v1 v1) ==> let
                      sep = getLMVSK $ F.fold foldLMVSKState (U.toList v1) <> F.fold foldLMVSKState (U.toList v2)
                      tog = F.fold fastLMVSK (U.toList v1 ++ U.toList v2)
                      in QC.counterexample (unlines ["",show sep,show tog, "Diff:", show (diffLMVSK sep tog)])
                        $ cmpLMVSK precision sep tog
                        || isNaN (lmvskKurtosis sep)
                        || isNaN (lmvskKurtosis tog)
                ]
            ]

        , testGroup "With pre-computed mean"
            [ testGroup "Functions requiring the mean to be known"
                [ onVec "variance" $ \vec ->
                    not (U.null vec) ==> let m = F.fold mean (U.toList vec)
                    in F.fold (variance m) (U.toList vec) == S.variance vec
                , onVec "varianceUnbiased" $ \vec ->
                    not (U.null vec) ==> let m = F.fold mean (U.toList vec)
                    in F.fold (varianceUnbiased m) (U.toList vec) == S.varianceUnbiased vec
                , onVec "stdDev" $ \vec ->
                    not (U.null vec) ==> let m = F.fold mean (U.toList vec)
                    in F.fold (stdDev m) (U.toList vec) == S.stdDev vec
                , onVec2 "varianceWeighted" $ \vec ->
                    not (U.null vec) ==> let m = F.fold meanWeighted (U.toList vec)
                    in F.fold (varianceWeighted m) (U.toList vec) == S.varianceWeighted vec
                ]

            , testGroup "Functions over central moments"
                [ onVec "skewness" $ \vec ->
                    U.length vec > 3 ==> let m = F.fold mean (U.toList vec)
                    in F.fold (skewness m) (U.toList vec) == S.skewness vec
                , onVec "kurtosis" $ \vec ->
                    U.length vec > 4 ==> let m = F.fold mean (U.toList vec)
                    in F.fold (kurtosis m) (U.toList vec) == S.kurtosis vec
                , onVec "centralMoment 2" $ \vec ->
                     U.length vec > 2 ==> let m = F.fold mean (U.toList vec)
                    in F.fold (centralMoment 2 m) (U.toList vec) == S.centralMoment 2 vec
                , onVec "centralMoment 3" $ \vec ->
                    U.length vec > 3 ==> let m = F.fold mean (U.toList vec)
                    in F.fold (centralMoment 3 m) (U.toList vec) == S.centralMoment 3 vec
                , onVec "centralMoment 4" $ \vec ->
                    U.length vec > 4 ==> let m = F.fold mean (U.toList vec)
                    in F.fold (centralMoment 4 m) (U.toList vec) == S.centralMoment 4 vec
                , onVec "centralMoment 7" $ \vec ->
                    U.length vec > 7 ==> let m = F.fold mean (U.toList vec)
                    in F.fold (centralMoment 7 m) (U.toList vec) == S.centralMoment 7 vec
                , onVec "centralMoments 4 9" $ \vec ->
                    U.length vec > 7 ==> let m = F.fold mean (U.toList vec)
                    in F.fold (centralMoments 4 9 m) (U.toList vec) == S.centralMoments 4 9 vec
                -- Cannot test this because we do not have an equivalent implementation
                -- from the statistics package.
                -- , onVec "centralMoments' 4 9" $ \vec -> length lst > 7 ==>
                --     let m = F.fold mean lst
                --         (f1,f2) = (F.fold (centralMoments' 4 9 m) lst)
                --         (s1,s2) = (S.centralMoments 4 9 vec)
                --     in within 3 f1 s1 && within 3 f2 s2
                ]
            , testGroup "Correlation"
                [ onVec2 "correlation between [-1,1]" $ \vec ->
                    U.length vec > 2 ==>
                    let m1 = F.fold mean (U.toList $ U.map fst vec)
                        m2 = F.fold mean (U.toList $ U.map snd vec)
                        s1 = F.fold (stdDev m1) (U.toList $ U.map fst vec)
                        s2 = F.fold (stdDev m2) (U.toList $ U.map snd vec)
                    in between (-1,1) $
                        F.fold (correlation (m1,m2) (s1,s2)) (U.toList vec)
                , onVec2 "correlation between [-1,1] fastStdDev" $ \vec ->

                    let (m1,m2) = F.fold ((,) <$> lmap fst mean <*> lmap snd mean)
                                        (U.toList vec)
                        (s1,s2) = F.fold ((,) <$> lmap fst (stdDev m1) <*> lmap snd (stdDev m2))
                                        (U.toList vec)
                        corr = F.fold (correlation (m1,m2) (s1,s2)) (U.toList vec)
                    in U.length vec > 2 && s2 /= 0.0 && s2 /= 0.0 ==>
                        QC.counterexample ("Correlation: " ++ show corr ++ " Stats: " ++ show (m1,m2,s1,s2)) $
                            between (-1,1) corr || isNaN corr
                , QC.testProperty "LinRegState Semigroup" $ \v1 v2 ->
                    U.length v1 > 2 && U.length v2 > 2
                    && U.sum (U.map fst (mappend v1 v1)) /= U.product (U.map fst (mappend v1 v1))
                    && U.sum (U.map snd (mappend v1 v1)) /= U.product (U.map snd (mappend v1 v1)) ==> let
                      sep = getLinRegResult $ F.fold foldLinRegState (U.toList v1) <> F.fold foldLinRegState (U.toList v2)
                      tog = F.fold fastLinearReg (U.toList v1 ++ U.toList v2)
                      in (cmpLMVSK precision (lrrXStats sep) (lrrXStats tog)
                         && cmpLMVSK precision (lrrYStats sep) (lrrYStats tog))
                        || isNaN (lmvskKurtosis (lrrXStats sep))
                        || isNaN (lmvskKurtosis (lrrYStats sep))
                        || isNaN (lmvskKurtosis (lrrXStats tog))
                        || isNaN (lmvskKurtosis (lrrYStats tog))
                ]
            ]
        ]

between :: (Double,Double) -> Double -> Bool
between (lo,hi) = \x -> lo <= x && x <= hi


withinPCT :: Double -> Double -> Double -> Bool
withinPCT pct a b = abs ((a - b) * 100 / (min `on` abs) a b)  < pct
