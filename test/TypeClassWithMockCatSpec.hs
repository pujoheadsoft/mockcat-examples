{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module TypeClassWithMockCatSpec (spec) where

import Data.Text (Text, pack, isInfixOf)
import Prelude hiding (writeFile, readFile)
import Test.Hspec (Spec, it, example, shouldBe)
import Test.MockCat as M
import Control.Monad (unless)
import Control.Monad.Reader

class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

makeMock [t|FileOperation|]

program1 ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  (Text -> Text) ->
  m ()
program1 inputPath outputPath modifyText = do
  content <- readFile inputPath
  let modifiedContent = modifyText content
  writeFile outputPath modifiedContent

program2 ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
program2 inputPath outputPath = do
  content <- readFile inputPath
  unless (pack "ngWord" `isInfixOf` content) $
    writeFile outputPath content

data Option = Option { checkNgWord :: Bool }

program3 ::
  MonadReader Option m =>
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
program3 inputPath outputPath = do
  (Option checkNgWord) <- ask
  content <- readFile inputPath
  unless (checkNgWord && (pack "ngWord" `isInfixOf` content) ) do
    writeFile outputPath content

makeMock [t|MonadReader Option|]

spec :: Spec
spec = do
  it "Read, edit, and output files" do
    example $ do
      modifyContentStub <- createStubFn $ pack "content" |> pack "modifiedContent"

      runMockT $ do
        _readFile $ "input.txt" |> pack "content"
        _writeFile $ "output.text" |> pack "modifiedContent" |> ()

        program1 "input.txt" "output.text" modifyContentStub

  it "読み込んだ内容にNGワードが含まれる場合、書き込みは行わない" do
    example $ do
      runMockT $ do
        _readFile $ "input.txt" |> pack "a ngWord!"
        neverApply $ _writeFile $ "output.text" |> M.any |> ()

        program2 "input.txt" "output.text"


  it "NGワードのチェックを行わない設定の場合、NGワードが含まれても、書き込みを行う" do
    example $ do
      runMockT $ do
        _ask $ Option False
        _readFile $ "input.txt" |> pack "a ngWord!"
        _writeFile $ "output.text" |> pack "a ngWord!" |> ()

        program3 "input.txt" "output.text"

  it "NGワードのチェックを行う設定で、NGワードが含まれる場合、書き込みを行わない" do
    example $ do
      runMockT $ do
        _ask $ Option True
        _readFile $ "input.txt" |> pack "a ngWord!"
        neverApply $ _writeFile $ "output.text" |> M.any |> ()

        program3 "input.txt" "output.text"

  it "NGワードのチェックを行う設定で、NGワードが含まれない場合、書き込みを行う" do
    example $ do
      runMockT $ do
        _ask $ Option True
        _readFile $ "input.txt" |> pack "content"
        _writeFile $ "output.text" |> pack "content" |> ()

        program3 "input.txt" "output.text"

  it "適用回数の検証が行える" do
    m <- createMock $ "arg1" |> "arg2" |> True
    stubFn m "arg1" "arg2" `shouldBe` True
    stubFn m "arg1" "arg2" `shouldBe` True
    m `shouldApplyTimes` (2 :: Int) $ "arg1" |> "arg2"
