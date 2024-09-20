{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module PolySemyTeletypeSpec (spec) where

import Data.Function ((&))
import Polysemy (Sem, interpret, makeSem, runM, Member, Embed, embed)
import Test.Hspec (Spec, it, shouldBe)
import Prelude hiding (readFile, writeFile)
import Test.MockCat

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo

spec :: Spec
spec = do
  it "Teletype" do
    readTTYStubFn <- createStubFn do 
      onCase $ pure @IO "output"
      onCase $ pure @IO ""
    writeTTYMock <- createMock $ "output" |> pure @IO ()

    let runEcho :: Member (Embed IO) r => Sem (Teletype : r) a -> Sem r a
        runEcho = interpret $ \case
          ReadTTY -> embed readTTYStubFn
          WriteTTY text -> embed $ stubFn writeTTYMock text

    result <-
      echo
        & runEcho
        & runM

    result `shouldBe` ()
    writeTTYMock `shouldApplyTo` "output"
