{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module ApiClientSpec where

import GHC.Generics
import Data.Aeson (decode, FromJSON)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest)
import Test.Hspec (Spec, it, shouldBe)
import Test.MockCat

-- レスポンスを処理するための型クラス
class Monad m => ApiClient m where
  makeRequest :: String -> m ByteString
  parseResponse :: ByteString -> m (Maybe Int)

-- APIを呼び出してレスポンスを解析する関数
fetchData :: ApiClient m => String -> m (Maybe Int)
fetchData url = do
  response <- makeRequest url
  parseResponse response

-- IOインスタンス（本物）
instance ApiClient IO where
  makeRequest url = do
    request <- parseRequest url
    response <- httpLBS request
    pure $ getResponseBody response

  -- JSONレスポンスから "value" フィールドの値を抽出
  parseResponse response = 
    case decode response of
      Just (ApiResponse obj) -> pure (Just obj)  -- レスポンスのパース結果を返す
      Nothing -> pure Nothing

-- JSONレスポンスの型（例）
newtype ApiResponse = ApiResponse { value :: Int }
  deriving (Generic, Show)

instance FromJSON ApiResponse

makePartialMock [t|ApiClient|]

-- テストで部分的にモックを使用
spec :: Spec
spec = do
  it "Fetch data with mocked response" do
    result <- runMockT do
      _makeRequest $ "https://example.com" |> BS.pack "{\"value\": 42}" -- モックされたJSONレスポンス
      fetchData "https://example.com"
    result `shouldBe` Just 42

  it "Handle invalid response" do
    result <- runMockT do
      _makeRequest $ "https://example.com" |> BS.pack "invalid response" -- パースできないデータ
      fetchData "https://example.com"
    result `shouldBe` Nothing
