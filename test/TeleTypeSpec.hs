{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
module TeleTypeSpec where

import Test.Hspec
import Test.MockCat
import Prelude hiding (writeFile, readFile, and, any, not, or)

class Monad m => Teletype m where
  readTTY :: m String
  writeTTY :: String -> m ()

echo :: Teletype m => m ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo

makeMockWithOptions [t|Teletype|] options { implicitMonadicReturn = False }

spec :: Spec
spec = do
  it "The process ends when an empty string is returned." do
    result <- runMockT do
      _readTTY $ pure @IO ""
      echo
    result `shouldBe` ()

  it "If a non-empty character is returned, it is recursed." do
    result <- runMockT do
      _readTTY $ do
        onCase $ pure @IO "a"
        onCase $ pure @IO ""

      _writeTTY $ "a" |> pure @IO ()
      echo
    result `shouldBe` ()