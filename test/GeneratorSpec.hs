module GeneratorSpec
  ( spec
  )
where
-- Dependency imports
import           Test.Hspec                     ( Spec
                                                , describe
                                                , parallel
                                                , it
                                                , shouldBe
                                                , shouldSatisfy
                                                )
import           Data.List                      ( all )
-- Project imports
import           Bank.Generator                 ( getRandomDoubles )

spec :: Spec
spec = parallel $ describe "getRandomDoubles" $ do

  it "Should produce a list of given length" $ do
    generatedList <- getRandomDoubles 1000
    generatedList `shouldSatisfy` (\list -> length list == 1000)

  it "Should only hold values between 0 and <1" $ do
    generatedList <- getRandomDoubles 1000
    generatedList `shouldSatisfy` all (\elem -> elem >= 0 && elem < 1)
