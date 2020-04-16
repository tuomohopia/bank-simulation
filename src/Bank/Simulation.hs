{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

module Bank.Simulation
  ( arrivalTimestamp
  , averageMaxWaitRandom
  , getQueueTimes
  , counterProcessingTime
  , averageMaxWait
  , Seconds
  , Probability
  , ArrivalTime
  , ProcessingTime
  , Customer(..)
  , Distribution(..)
  )
where
-- Dependency imports
import           GHC.Float                      ( int2Double )
import           Data.List                      ( mapAccumL )
-- Project imports
import           Bank.Generator                 ( getRandomDoubles )

import           Debug.Trace

-- * Constants and Configs

-- | Given constant in the assignment
p :: Double
p = 200

-- | Retrieval for the given Alpha & Beta values for each customer color.
instance Distribution Customer where
  getDist Yellow = (2, 5)
  getDist Red    = (2, 2)
  getDist Blue   = (5, 1)

-- * Types, Constraints and Aliases

type Seconds = Double
type SampleSize = Int
type WaitingTimes = [Seconds]

type Alpha = Double
type Beta = Double

type Average = Seconds
type Max = Seconds
type ArrivalTime = Seconds
type ProcessingTime = Seconds
type QueueTime = Seconds

-- | Due to the requirement of being between 0 and <1
-- this is better represented as a 'newtype' with a smart constructor.
-- However, for this small assignment it would add unnecessary boilerplate.
type Probability = Double

data Customer = Yellow | Red | Blue
    deriving (Show, Eq)

-- Just a way to unify fetching distribution parameters for different types.
-- When expanding the project, we could use 'Distribution' class as a constraint
-- to show that the type needs to provide its related distribution Alpha and Beta values.
class Distribution a where
  getDist :: a -> (Alpha, Beta)


-- * Challenges

-- ** Task 1: Given only yellow customers, what are the average and maximum customer waiting times?

-- | Average and Max Queuing times with Monte Carlo approximation.
averageMaxWaitRandom :: Distribution a => a -> Int -> IO (Average, Max)
averageMaxWaitRandom member sampleCount = do
  let dist = getDist member
  randomsArrival    <- map arrivalTimestamp <$> getRandomDoubles sampleCount
  -- Get a new random list for processing times
  randomsProcessing <- map (counterProcessingTime dist)
    <$> getRandomDoubles sampleCount
  return $ averageMaxWait (randomsArrival, randomsProcessing)


-- * Lib functions

-- | Computes average & max wait times for given probability lists & member's alpha/beta values.
averageMaxWait :: ([ArrivalTime], [ProcessingTime]) -> (Average, Max)
averageMaxWait (randomsArrival, randomsProcessing) =
  let combined   = zip randomsArrival randomsProcessing
      queueTimes = getQueueTimes combined
  in  (sum queueTimes / n, maximum queueTimes)
  where n = int2Double $ length randomsArrival -- sampleCount

-- | Builds a list of total queuing times based on arrival & processing times.
getQueueTimes :: [(ArrivalTime, ProcessingTime)] -> WaitingTimes
getQueueTimes []        = []
getQueueTimes timePairs = snd $ mapAccumL queueAccumulator initialAcc timePairs
  where initialAcc = fst $ head timePairs -- Set the initial accumulator as the first one's arrival.

-- | Build the actual queue wait time list.
-- The 'acc' accumulator is the total waiting time at each point
-- If the person arrives later than 
queueAccumulator
  :: Seconds -> (ArrivalTime, ProcessingTime) -> (Seconds, QueueTime)
queueAccumulator currentQueue (arr, proc) =
  let hasToWait = currentQueue > arr
      newQueue  = if hasToWait then currentQueue - arr + proc else proc
      waitTime  = if hasToWait then currentQueue - arr else 0
  in  (newQueue, waitTime)

-- | Computes the counter processing time of a group member (customer).
-- Using formula: G(x) = p ⋅ (xα−1) ⋅ ((1−x)β−1)
-- NOTE: Throws error if x >= 1 or x < 0.
counterProcessingTime :: (Alpha, Beta) -> Probability -> Seconds
counterProcessingTime (alpha, beta) x
  | randomWithinLimits x = p * (x ** (alpha - 1)) * ((1 - x) ** (beta - 1))
  | otherwise            = error $ "Faulty random input number " ++ show x


-- Ideally, we need the arrival timestamp so that we can combine that with processing times
-- to calculate average and maximum waiting times. Therefore, instead of calculating the probabilities
-- of the arrivals, we can attempt to substitute the probabilities by randomly generated ones
-- and calculate arrival timestamps instead. With Monte Carlo algorithm we can then approximate
-- the average and maximum wait times.

-- | Since we know that:
-- * t = seconds since the last customer arrived.
-- * F(t) = Probability that a customer arrives this second, result from 0 and approaching 1.
-- 
-- We can solve for `t` which yields:
-- @
--    t = ln (1 - F(t)) ⋅ (-α)
-- @
-- 
-- Now by substituting F(t) by a randomly generated value from the same range,
-- we can compute the actual arrival timestamp of the group member.
-- NOTE: Natural logarithm in Haskell is just 'log': https://mail.haskell.org/pipermail/haskell-cafe/2013-January/105659.html
arrivalTimestamp :: Probability -> Seconds
arrivalTimestamp prob
  | randomWithinLimits prob = log (1 - prob) * (-100)
  | otherwise               = error $ "Faulty random input number " ++ show prob

-- | Internal helper to constrain the 'Probability' to between 0 and <1.
randomWithinLimits :: Probability -> Bool
randomWithinLimits x = x >= 0 && x < 1
