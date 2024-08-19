{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TypeClassWithMockCatSpec (spec) where

import Data.Text (Text, pack)
import Prelude hiding (writeFile, readFile)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat (makeMock, createStubFn, (|>), runMockT)

class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

makeMock [t|FileOperation|]

program ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
program inputPath outputPath modifyText = do
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent

spec :: Spec
spec = do
  it "Read, edit, and output files" do
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    result <- runMockT $ do
      _readFile $ "input.txt" |> pack "content"
      _writeFile $ "output.text" |> pack "modifiedContent" |> ()

      program "input.txt" "output.text" modifyContentStub

    result `shouldBe` ()
