module Bank.Simulation where
-- Project imports
import           Bank.Generator                 ( getRandomDoubles )

-- * Constants and Configs

-- | Given constant in the assignment
p :: Double
p = 200

-- * Types and Aliases

data Customer = Yellow | Red | Blue
    deriving (Show, Eq)

-- | Retrieve the given Alpha & Beta values for each customer color
instance Distribution Customer where
  getDist Yellow = (2, 5)
  getDist Red    = (2, 2)
  getDist Blue   = (5, 1)

-- Just a way to unify fetching distribution parameters for different types.
-- When expanding the project, we could use 'Distribution' class as a constraint
-- to show that the type needs to provide its related distribution Alpha and Beta values.
class Distribution a where
  getDist :: a -> (Alpha, Beta)

type Seconds = Double
type Interval = Double
type SampleSize = Int
type WaitingTimes = [Seconds]

type Alpha = Double
type Beta = Double


-- * Challenges

-- ** Task 1: Given only yellow customers, what are the average and maximum customer waiting times?


-- * Lib functions

-- | Computes the processing time of a customer.
-- Using formula: G(x) = p ⋅ (xα−1) ⋅ ((1−x)β−1)
-- NOTE: Throws error if x >= 1 or x < 0.
processingTime
  :: (Alpha, Beta) -- ^ Alpha, beta values
  -> Double -- ^ Random value; 0 <= value > 1.
  -> Seconds -- ^ Processing time for customer
processingTime (alpha, beta) x
  | randomWithinLimits = p * (x ** (alpha - 1)) * ((1 - x) ** (beta - 1))
  | otherwise          = error $ "Faulty random input number " ++ show x
  where randomWithinLimits = x >= 0 && x < 1
