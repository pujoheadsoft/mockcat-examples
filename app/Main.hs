module Main (main) where

import Test.MockCat (createStubFn, (|>))
import Control.Monad.Reader (Reader, ask, runReader)
import Data.Function ((&))

main :: IO ()
main = do
  f <- createStubFn $ "input" |> do
    e <- ask
    pure @(Reader String) $ "env is: " <> e
  
  f "input"
    & flip runReader "option value"
    & print -- "env is: option value"
