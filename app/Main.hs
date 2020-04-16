module Main where
-- Project imports
import           Bank.Simulation                ( averageMaxWaitYellow )

main :: IO ()
main = do
  putStrLn "Task 1: Yellow customers"
  (avg, max) <- averageMaxWaitYellow
  putStrLn $ "Average waiting times: " ++ show avg ++ " seconds"
  putStrLn $ "Max waiting times: " ++ show max ++ " seconds"
