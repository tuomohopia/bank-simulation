module Bank.Generator
  ( getRandomDoubles
  )
where
-- Dependency imports
import           Test.QuickCheck.Gen            ( generate
                                                , vectorOf
                                                , suchThat
                                                , choose
                                                , Gen
                                                )

-- * Public API

-- | Builds a list of doubles of supplied length.
getRandomDoubles :: Int -> IO [Double]
getRandomDoubles sampleCount = generate $ vectorOf sampleCount singleRandom


-- * Internal

-- | Generate values between 0 and <1.
singleRandom :: Gen Double
singleRandom = suchThat baseGenerator (\a -> a < 1)
  where baseGenerator = choose (0, 1)
