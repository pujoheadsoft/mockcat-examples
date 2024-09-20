[ANN] モックライブラリmockcatにモナド型クラスのモック作成機能を追加しました

こんにちは、mockcatの開発者です。  
mockcatは、Haskellで簡単にモックを作成し、テストをサポートするためのライブラリです。  
今回、mockcatに新しい機能を追加しました。それは、モナド型クラスのモックを作成できる機能です。

v0.2.1.0までのmockcatでは、主に純粋関数のモックのみをサポートしていたため型クラスのモックを利用したテストを書くためには、他のライブラリと組み合わせる必要がありました。
しかしv0.3.1.0からはモナド型クラスのモックもサポートするようになったため、モックを利用したテストはこのライブラリで完結できるようになるはずです。

**モナド型クラスのモックの例**  
この新機能を利用することで、特定の関数適用に対して期待する結果を返すようにモックを設定できます。以下に簡単な例を示します。  
例えば次のような型クラスとテスト対象の関数があるとします。
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
型クラス`FileOperation`に対して、`makeMock`関数を適用させることでモックを生成することができます。  
モックを利用したテストは次のようになります。
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
モックを生成すると、`_`から始まる関数が生成されますが、これが期待値を返す関数です。  
生成する関数の名前はオプションである程度変えることができます(詳しくはドキュメントを参照してください)。

**まとめ**  
この新機能により、Haskellでモナドを使用したコードのテストがより簡単になります。  
興味のある方は、mockcatのGitHubリポジトリで詳細を確認し、ぜひ試してみてください。フィードバックや質問もお待ちしております！

**今後**  
いまは部分的なモックができるよう開発を進めています。  
これができれば、型クラスの特定の関数だけスタブにできるようになります。

**おまけ**  
最後に、`MonadReader`のモックを使ったより複雑な例を載せておきます。
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