module Main where
-- Project imports
import           Bank.Simulation                ( averageMaxWaitRandom
                                                , Customer(..)
                                                )

main :: IO ()
main = do
  putStrLn "Task 1: Yellow customers"
  (avg, max) <- averageMaxWaitRandom Yellow 5
  putStrLn $ "Average waiting times: " ++ show avg ++ " seconds"
  putStrLn $ "Max waiting times: " ++ show max ++ " seconds"
