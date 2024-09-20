[ANN] Mocking function for monad type classes is added to mockcat, a mock library.

Hello, I'm the developer of mockcat.
mockcat is a library designed to simplify the creation of mocks in Haskell and support testing.
This time, we've added a new feature to mockcat that allows you to create mocks for monad type classes.

Up to v0.2.1.0, mockcat mainly supported only pure function mocking, so tests using mocking of typeclasses had to be combined with other libraries.
However, since v0.3.1.0, mockcat now also supports mocking of monadic type classes, so tests using mocks should be complete with this library.

Example of Monad Type Class Mocks
With this new feature, you can set up mocks that return the expected results for specific function applications. Here's a simple example:
Let's say you have the following type class and function to be tested.

```haskell
import Prelude hiding (writeFile, readFile)

class Monad m => FileOperation m where
  readFile :: FilePath -> m Text
  writeFile :: FilePath -> Text -> m ()

program ::
  FileOperation m =>
  FilePath ->
  FilePath ->
  m ()
program inputPath outputPath = do
  content <- readFile inputPath
  writeFile outputPath content
```
By applying the makeMock function to the FileOperation type class, you can generate mocks.
The test using mocks would look like this:
```haskell
makeMock [t|FileOperation|]

spec :: Spec
spec = do
  it "Read and output files" do
    example $ do
      runMockT $ do
        _readFile $ "input.txt" |> pack "content"
        _writeFile $ "output.text" |> pack "content" |> ()

        program "input.txt" "output.text" 
```
When a mock is generated, functions starting with _ are created. These functions return the expected values.
You can also customize the names of these generated functions to some extent (see the documentation for details).

Summary
This new feature makes testing code that uses monads in Haskell even easier.
If you're interested, please check out the mockcat GitHub repository for more details and give it a try. Feedback and questions are always welcome!

Future Plans
Currently, I'm working on allowing partial mocks.
Once this is implemented, you'll be able to stub specific functions within a type class.

Bonus
Lastly, here's a more complex example using mocks with MonadReader.
```haskell
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
```