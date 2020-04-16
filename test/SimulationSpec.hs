{-# LANGUAGE ScopedTypeVariables #-}
module SimulationSpec
  ( spec
  )
where
-- Dependency imports
import           Test.Hspec                     ( Spec
                                                , describe
                                                , parallel
                                                , it
                                                , shouldMatchList
                                                , shouldSatisfy
                                                , shouldBe
                                                , context
                                                )
import           Data.List                      ( all )
import           GHC.Float                      ( double2Float )
-- Project imports
import           Bank.Generator                 ( getRandomDoubles )
import           Bank.Simulation                ( arrivalTimestamp
                                                , getQueueTimes
                                                , averageMaxWait
                                                , Seconds
                                                , Probability
                                                , ArrivalTime
                                                , ProcessingTime
                                                )

spec :: Spec
spec = describe "Simulation Tests" $ do

    -- NOTE: This fails if the doubles are not rounded with float conversion. For some reason
    -- it will otherwise sometimes produce a few marginally different numbers.
    -- The difference is about 0.0000000000000001% but enough to make the test fail.
  context "arrivalTimestamp" $ do
    it "Should mathematically equal the original function" $ do
      randomProbabilities <- getRandomDoubles 100
      let arrivals                  = map arrivalTimestamp randomProbabilities
          probabilitiesFromArrivals = map originalFormula arrivals
          result                    = map double2Float probabilitiesFromArrivals
          expected                  = map double2Float randomProbabilities
      result `shouldMatchList` expected

  context "getQueueTimes" $ do
    it "Should compute queue times correctly" $ do
      let q1 :: [(ArrivalTime, ProcessingTime)]
          q1 = [(0, 10), (5, 5), (3, 10)]
          q2 :: [(ArrivalTime, ProcessingTime)]
          q2 = [(10, 6), (1, 100), (50, 1)]
          q3 = []
      getQueueTimes q1 `shouldMatchList` [0, 5, 7]
      getQueueTimes q2 `shouldMatchList` [0, 5, 55]
      getQueueTimes q3 `shouldMatchList` []

  context "averageMaxWait" $ do
    it "Should produce correct averages and maximums" $ do
      let a1 = [5.0, 5.0, 5.0]
          p1 = [0.0, 0.0, 0.0]
          a2 = [0.0, 5.0, 1.0]
          p2 = [10.0, 15.0, 0.0]
      averageMaxWait (a1, p1) `shouldBe` (0.0, 0.0)

-- | Original provided math formula, used for property testing.
originalFormula :: Seconds -> Probability
originalFormula t = 1 - exp (-t / 100)
