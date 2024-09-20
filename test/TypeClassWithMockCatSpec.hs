{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TypeClassWithMockCatSpec (spec) where

import Data.Text (Text, pack, isInfixOf)
import Prelude hiding (writeFile, readFile)
import Test.Hspec (Spec, it, example)
import Test.MockCat as M
import Control.Monad (unless)
import Control.Monad.Reader

class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

data Option = Option { checkNgWord :: Bool }

makeMock [t|FileOperation|]
makeMock [t|MonadReader Option|]

program ::
  MonadReader Option m =>
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
program inputPath outputPath = do
  (Option checkNgWord) <- ask
  content <- readFile inputPath
  unless (checkNgWord && (pack "ngWord" `isInfixOf` content) ) do
    writeFile outputPath content

spec :: Spec
spec = do
  it "If the setting does not check for NG words, write even if NG words are included." do
    example $ do
      runMockT $ do
        _ask $ Option False
        _readFile $ "input.txt" |> pack "a ngWord!"
        _writeFile $ "output.text" |> pack "a ngWord!" |> ()

        program "input.txt" "output.text"

  it "Set to check for NG words, and if NG words are included, do not write them down." do
    example $ do
      runMockT $ do
        _ask $ Option True
        _readFile $ "input.txt" |> pack "a ngWord!"
        neverApply $ _writeFile $ "output.text" |> M.any |> ()

        program "input.txt" "output.text"

  it "If the setting is set to check for NG words, write if NG words are not included." do
    example $ do
      runMockT $ do
        _ask $ Option True
        _readFile $ "input.txt" |> pack "content"
        _writeFile $ "output.text" |> pack "content" |> ()

        program "input.txt" "output.text"
