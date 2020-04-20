module Main where
-- Dependency imports
import           Data.Text.Format               ( fixed )
-- Project imports
import           Bank.Simulation                ( averageMaxWaitRandom
                                                , averageMaxQueueRandom
                                                , Customer(..)
                                                )

main :: IO ()
main = do
  -- TASK 1
  putStrLn "Task 1: Yellow customers - computing queue waiting times"
  (avg1, max1) <- averageMaxWaitRandom Yellow 100000
  putStrLn $ "Average waiting times: " ++ formatDouble avg1 ++ " seconds"
  putStrLn $ "Max waiting times: " ++ formatDouble max1 ++ " seconds"
  -- TASK 2
  putStrLn "Task 2: Red customers - computing queue lengths"
  (avg2, max2) <- averageMaxQueueRandom Red 10000
  putStrLn $ "Average queue length: " ++ formatDouble avg2 ++ " customers"
  putStrLn $ "Maximum queue length: " ++ show max2 ++ " customers"

-- | Pretty printing helper
formatDouble :: Real a => a -> String
formatDouble d =
  let formatted = show $ fixed 3 d in filter (\a -> a /= '"') formatted
