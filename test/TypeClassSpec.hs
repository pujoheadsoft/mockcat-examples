{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

module TypeClassSpec (spec) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Text (Text, pack)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat (createMock, createStubFn, stubFn, (|>), shouldApplyTo)
import Prelude hiding (readFile, writeFile)

class (Monad m) => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

program ::
  (FileOperation m) =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
program inputPath outputPath modifyText = do
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent

data Functions = Functions
  { _readFile :: FilePath -> Text,
    _writeFile :: FilePath -> Text -> ()
  }

instance Monad m => FileOperation (ReaderT Functions m) where
  readFile path = ask >>= \f -> pure $ f._readFile path
  writeFile path content = ask >>= \f -> pure $ f._writeFile path content

spec :: Spec
spec = do
  it "Read, edit, and output files" do
    readFileStub <- createStubFn $ "input.txt" |> pack "content"
    writeFileMock <- createMock $ "output.text" |> pack "modifiedContent" |> ()
    modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

    let functions =
          Functions
            { _readFile = readFileStub,
              _writeFile = stubFn writeFileMock
            }

    result <-
      runReaderT
        (program "input.txt" "output.text" modifyContentStub)
        functions

    result `shouldBe` ()
    writeFileMock `shouldApplyTo` ("output.text" |> pack "modifiedContent")
