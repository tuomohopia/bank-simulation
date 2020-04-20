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
                                                , howManyQueueing
                                                , Seconds
                                                , ArrivalTime
                                                , ProcessingTime
                                                )
import           Bank.Probability               ( Probability
                                                , probability
                                                , getProb
                                                )


spec :: Spec
spec = parallel $ describe "Simulation Tests" $ do

  -- NOTE: This fails if the doubles are not rounded with float conversion. For some reason
  -- it will otherwise sometimes produce a few marginally different numbers.
  -- The difference is about 0.0000000000000001% but enough to make the test fail.
  context "arrivalTimestamp" $ do
    it "Should mathematically equal the original function" $ do
      randomProbabilities <- map probability <$> getRandomDoubles 100
      let arrivals = map arrivalTimestamp randomProbabilities
          probabilitiesFromArrivals = map originalFormula arrivals
          result = map (double2Float . getProb) probabilitiesFromArrivals
          expected = map (double2Float . getProb) randomProbabilities
      result `shouldMatchList` expected

  context "getQueueTimes" $ do
    it "Should compute queue times correctly" $ do
      let q1 :: [(ArrivalTime, ProcessingTime)]
          q1 = [(0, 10), (5, 5), (3, 10)]
          q2 :: [(ArrivalTime, ProcessingTime)]
          q2 = [(10, 6), (1, 100), (50, 1)]
          q3 :: [(ArrivalTime, ProcessingTime)]
          q3 = [(10, 16)]
          q4 = []
      getQueueTimes q1 `shouldMatchList` [0, 5, 7]
      getQueueTimes q2 `shouldMatchList` [0, 5, 55]
      getQueueTimes q3 `shouldMatchList` [0]
      getQueueTimes q4 `shouldMatchList` []

  context "averageMaxWait" $ do
    it "Should produce correct averages and maximums" $ do
      -- Happy case test
      let a1 = [5.0, 5.0, 5.0]
          p1 = [0.0, 0.0, 0.0]
      averageMaxWait (a1, p1) `shouldBe` (0.0, 0.0)

  context "howManyQueueing" $ do
    it "Should compute the number of customers queuing correctly" $ do
      -- Happy case test
      let timestamps =
            [(0.0, 0.0), (5.0, 8.0), (6.0, 12.0), (15.0, 17.0), (16.0, 18.0)]
          end = 18.0
          at1 = 0.3
          at2 = 7.0
          at3 = 10.0
          at4 = 16.2
      howManyQueueing at1 end timestamps `shouldBe` 0
      howManyQueueing at2 end timestamps `shouldBe` 2
      howManyQueueing at3 end timestamps `shouldBe` 1
      howManyQueueing at4 end timestamps `shouldBe` 2

-- | Original provided math formula, used for property testing.
originalFormula :: Seconds -> Probability
originalFormula t = probability $ 1 - exp (-t / 100)
