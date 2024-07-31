{-# LANGUAGE BlockArguments #-}
module SimpleCaseSpec (spec) where
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat (createStubFn, (|>))

checkLength :: (String -> Bool) -> String -> String
checkLength isLong s =
  if isLong s
    then "Long enough."
    else "Too short."

spec :: Spec
spec = do
  it "if long" do
    f <- createStubFn $ "str" |> True
    checkLength f "str" `shouldBe` "Long enough."
  
  it "if short" do
    f <- createStubFn $ "str" |> False
    checkLength f "str" `shouldBe` "Too short."