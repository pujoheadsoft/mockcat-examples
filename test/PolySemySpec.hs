{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module PolySemySpec where

import Polysemy (Sem, interpret, makeSem, Member, Embed, embed, runM)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat (createMock, (|>), stubFn, shouldApplyTo, createStubFn)

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo

-- main :: IO ()
-- main = runM . teletypeToIO $ echo

spec :: Spec
spec = do
  it "" do
    writeMock <- createMock $ "user input" |> ()

    let
      runTeletype :: Sem (Teletype : r) a -> Sem r a
      runTeletype = interpret $ \case
        ReadTTY -> pure "user input"
        WriteTTY msg -> pure $ stubFn writeMock msg

    _ <- runM . runTeletype $ echo

    writeMock `shouldApplyTo` "user input"