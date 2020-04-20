{-# LANGUAGE ScopedTypeVariables #-}

module Bank.Simulation
  ( arrivalTimestamp
  , averageMaxWaitRandom
  , getQueueTimes
  , counterProcessingTime
  , averageMaxWait
  , averageMaxQueueRandom
  , howManyQueueing
  , closestAvgMaxCustomer
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
import           Data.List                      ( mapAccumL
                                                , minimumBy
                                                )
-- Project imports
import           Bank.Generator                 ( getRandomDoubles )
import           Bank.Probability               ( Probability
                                                , probability
                                                , getProb
                                                )

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

data Customer = Yellow | Red | Blue
    deriving (Show, Eq)

type Seconds = Double
type SampleSize = Int

type Alpha = Double
type Beta = Double

type Average = Seconds
type Max = Seconds
type ArrivalTime = Seconds
type ProcessingTime = Seconds
type QueueTime = Seconds
type QueueTimes = [QueueTime]

type StartTimestamp = Seconds -- Absolute time when customer enters the queue
type EndTimestamp = Seconds -- Absolute time when customer leaves the queue
-- | NOTE: This is a simple tuple due to performance reasons.
-- The generated list operated on in this program can be large.
-- Representing this data type as a simple tuple allows the GHC to optimize
-- unpacking when its contained data types are single constructor as well,
-- when passed to a strict function.
-- Otherwise, an ADT with `enter` and `exit` records would serve better.
type QueueTimestamps = (StartTimestamp, EndTimestamp)

type AverageQueue = Double
type MaxQueue = Int

-- | Just a way to unify fetching distribution parameters for different types.
-- When expanding the project, we could use 'Distribution' class as a constraint
-- to show that the type needs to provide its related distribution Alpha and Beta values.
class Distribution a where
  getDist :: a -> (Alpha, Beta)


-- * Challenges

-- ** Task 1: Given only yellow customers, what are the average and maximum customer waiting times?

-- | Average and Max Queuing times with Monte Carlo approximation.
averageMaxWaitRandom :: Distribution a => a -> Int -> IO (Average, Max)
averageMaxWaitRandom member sampleCount = do
  randomsArrival    <- map getArrival <$> getRandomDoubles sampleCount
  -- Get a new random list for processing times
  randomsProcessing <- map (getProcessing member)
    <$> getRandomDoubles sampleCount
  return $ averageMaxWait (randomsArrival, randomsProcessing)


-- ** Task 2: Given only red customers, what are the average and maximum queue lengths in-front of the teller?
{- 
My understanding of the question phrasing is that the idea is to calculate
how many people in the queue at by specific intervals, 
and then calculate the average and maximum from these.

We know for each customer:
* Bank entering time
* Processing time

From these we're able to derive:
* Queue wait times of each customer
* Timestamps of customers entering and leaving

Thus, we need to get everyone's queuing start and end times.
These times need to be absolute, not relative to the previous customer.

With this, we can check the number of customers queuing at any given time.
 -}

averageMaxQueueRandom
  :: Distribution a => a -> Int -> IO (AverageQueue, MaxQueue)
averageMaxQueueRandom member sampleCount = do
  randomsArrival    <- map getArrival <$> getRandomDoubles sampleCount
  -- Get a new random list for processing times
  randomsProcessing <- map (getProcessing member)
    <$> getRandomDoubles sampleCount
  return $ averageMaxQueue (randomsArrival, randomsProcessing)


-- ** Task 3: Which type of customer(yellow, red or blue) gives the gives the closest value between the average and maximum customer waiting times?

closestAvgMaxCustomer :: Int -> IO Customer
closestAvgMaxCustomer sampleCount = do
  (yellowAvg, yellowMax) <- averageMaxQueueRandom Yellow sampleCount
  (redAvg   , redMax   ) <- averageMaxQueueRandom Red sampleCount
  (blueAvg  , blueMax  ) <- averageMaxQueueRandom Blue sampleCount
  -- average and max are both positive numbers, and avg should always be <= max
  let yellowWithDiff = (Yellow, int2Double yellowMax - yellowAvg)
      redWithDiff    = (Red, int2Double redMax - redAvg)
      blueWithDiff   = (Blue, int2Double blueMax - blueAvg)
      -- Get and return the customer color with the smallest difference
      minDiff        = minimumBy (\a b -> compare (snd a) (snd b))
                                 [yellowWithDiff, redWithDiff, blueWithDiff]
  return $ fst minDiff


-- * Lib functions
-- Keep all business logic functions pure so they can be deterministically tested

-- | Computes the average and maximum queue lengths.
-- Calculates both based on a fixed interval at which to measure.
-- Interval is total time / 'parts' from config at the top.
-- 
-- NOTE: This is not optimized for performance.
-- Lists of doubles are probably the wrong data structure here for high performance.
averageMaxQueue :: ([ArrivalTime], [ProcessingTime]) -> (AverageQueue, MaxQueue)
averageMaxQueue times@(arrivalTimes, processingTimes) =
  let
    parts        = length arrivalTimes * 10 -- slice to 10x more intervals than there are customers
    combined     = zip arrivalTimes processingTimes
    endTime      = getEndTime combined -- same as total time
    queueTimes   = getAbsoluteQueueTimes combined -- (Enter, Exit) in absolute timestamps
    timeSlice    = endTime / int2Double parts -- One time interval
    allIntervals = takeWhile (< endTime) [0, timeSlice ..] -- all intervals in a list
    queueLengths =
      map (\at -> howManyQueueing at endTime queueTimes) allIntervals -- the count of users at each interval end time
    queueLengthsD = map int2Double queueLengths -- convert to doubles
    avg           = sum queueLengthsD / (int2Double . length) queueLengthsD
  in
    (avg, maximum queueLengths)

-- | Fetches the amount of people queuing at the given timestamp.
-- 'QueueTimestamps' holds customer's queue entering and exit times.
-- If the timestamp is before or beyond the last queuing
-- 
-- NOTE: Profiling reveals 'closestAvgMaxCustomer' spends about 97% of its time
-- in this function's invocations. A more efficient data structure for Queue Timestamps
-- would be something more specific to interval lookups like Map/Set:
-- https://hackage.haskell.org/package/IntervalMap
-- However, for this assignment refactoring would come at the expense of code clarity.
howManyQueueing :: Seconds -> Seconds -> [QueueTimestamps] -> Int
howManyQueueing at end timestamps
  | at >= 0 && at < end = length $ filter isAtQueue timestamps
  | otherwise           = error "Timestamp beyond the bank opening hours"
  where isAtQueue (arr, exit) = at >= arr && at < exit -- between arrival and exit


-- | Fetches the absolute end timestamp of the customer processing.
-- This is done by taking the timestamp of when the last customer enters processing
-- and adding his processing time.
getEndTime :: [(ArrivalTime, ProcessingTime)] -> Seconds
getEndTime [] = 0
getEndTime times =
  let lastQueueEndTimestamp = snd $ last $ getAbsoluteQueueTimes times
      lastProcessingTime    = snd $ last times
  in  lastQueueEndTimestamp + lastProcessingTime

-- | Builds the list of absolute queuing timestamps of each customer.
getAbsoluteQueueTimes :: [(ArrivalTime, ProcessingTime)] -> [QueueTimestamps]
getAbsoluteQueueTimes [] = [] -- this makes 'head' safe at 'initialAcc'
getAbsoluteQueueTimes times =
  let queueingDurations = getQueueTimes times
      zipper (arr, proc) que = (arr, que, proc) -- helper fn
      fst3 (x, _, _) = x -- helper fn
      arrivalQueues :: [(ArrivalTime, QueueTime, ProcessingTime)] -- relative (to previous) times
      arrivalQueues = zipWith zipper times queueingDurations
      initialAcc    = fst3 $ head arrivalQueues -- Start counting time from the first's arrival
  in  snd $ mapAccumL timestampAccumulator initialAcc arrivalQueues -- absolute times 

-- | Accumulator to build Queue Timestamps with enter and exit queue times.
timestampAccumulator
  :: Seconds
  -> (ArrivalTime, QueueTime, ProcessingTime)
  -> (Seconds, QueueTimestamps)
timestampAccumulator currentTime (arr, que, proc) =
  (accumulatedTime, (enterQueueTime, exitQueueTime))
 where
  accumulatedTime = currentTime + arr + que + proc
  enterQueueTime  = currentTime + arr
  exitQueueTime   = currentTime + arr + que

-- | Computes average & max queuing times based on given arrivals & processing times.
averageMaxWait :: ([ArrivalTime], [ProcessingTime]) -> (Average, Max)
averageMaxWait (arrivalTimes, processingTimes) =
  (sum queueTimes / sampleCount, maximum queueTimes)
 where
  queueTimes  = getQueueTimes $ zip arrivalTimes processingTimes
  sampleCount = int2Double $ length arrivalTimes

-- | Builds a list of total queuing times (duration) based on arrival & processing times.
-- The first person to arrive has no waiting time at all.
getQueueTimes :: [(ArrivalTime, ProcessingTime)] -> QueueTimes
getQueueTimes []        = []
getQueueTimes timePairs = snd $ mapAccumL queueAccumulator initialAcc timePairs
  where initialAcc = fst $ head timePairs -- Set the initial accumulator as the first one's arrival.

-- | Build the actual queue wait time list.
-- The 'acc' accumulator is the total waiting time at each point
-- If the person arrives later than what the length (seconds) of the current queue is,
-- he/she has no queuing time at all. 
queueAccumulator
  :: Seconds -> (ArrivalTime, ProcessingTime) -> (Seconds, QueueTime)
queueAccumulator currentQueue (arr, proc) =
  let hasToWait = currentQueue > arr
      newQueue  = if hasToWait then currentQueue - arr + proc else proc
      waitTime  = if hasToWait then currentQueue - arr else 0
  in  (newQueue, waitTime)

-- | Computes the counter processing time of a group member (customer).
-- Using formula: G(x) = p ⋅ (xα−1) ⋅ ((1−x)β−1)
counterProcessingTime :: (Alpha, Beta) -> Probability -> Seconds
counterProcessingTime (alpha, beta) prob =
  p * (x ** (alpha - 1)) * ((1 - x) ** (beta - 1))
  where x = getProb prob


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
arrivalTimestamp prob = log (1 - x) * (-100) where x = getProb prob

-- | Helper
getArrival :: Seconds -> Seconds
getArrival = arrivalTimestamp . probability

-- | Helper
getProcessing :: Distribution a => a -> Seconds -> Seconds
getProcessing member = counterProcessingTime (getDist member) . probability
