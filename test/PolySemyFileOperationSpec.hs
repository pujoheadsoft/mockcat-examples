{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module PolySemyFileOperationSpec (spec) where

import Data.Function ((&))
import Data.Text
import Polysemy (Sem, interpret, makeSem, runM, Member)
import Test.Hspec (Spec, it, shouldBe)
import Prelude hiding (readFile, writeFile)
import Test.MockCat

data FileOperation m a where
  ReadFile :: FilePath -> FileOperation m Text
  WriteFile :: FilePath -> Text -> FileOperation m ()

makeSem ''FileOperation

program ::
  Member FileOperation r =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  Sem r ()
program inputPath outputPath modifyText = do
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent

spec :: Spec
spec = do
  it "Read, edit, and output files" do
    readFileStub <- createStubFn $ "input.txt" |> pack "content"
    writeFileMock <- createMock $ "output.text" |> pack "modifiedContent" |> ()
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    let runFileOperation :: Sem (FileOperation : r) a -> Sem r a
        runFileOperation = interpret $ \case
          ReadFile path -> pure $ readFileStub path
          WriteFile path text -> pure $ stubFn writeFileMock path text

    result <-
      program "input.txt" "output.text" modifyContentStub
        & runFileOperation
        & runM

    result `shouldBe` ()

    writeFileMock `shouldApplyTo` ("output.text" |> pack "modifiedContent")