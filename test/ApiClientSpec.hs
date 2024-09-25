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

class Monad m => ApiClient m where
  makeRequest :: String -> m ByteString
  parseResponse :: ByteString -> m (Maybe Int)

fetchData :: ApiClient m => String -> m (Maybe Int)
fetchData url = do
  response <- makeRequest url
  parseResponse response

instance ApiClient IO where
  makeRequest url = do
    request <- parseRequest url
    response <- httpLBS request
    pure $ getResponseBody response

  parseResponse response = 
    case decode response of
      Just (ApiResponse obj) -> pure (Just obj)
      Nothing -> pure Nothing

newtype ApiResponse = ApiResponse { value :: Int }
  deriving (Generic, Show)

instance FromJSON ApiResponse

makePartialMock [t|ApiClient|]

spec :: Spec
spec = do
  it "Fetch data with mocked response" do
    result <- runMockT do
      _makeRequest $ "https://example.com" |> BS.pack "{\"value\": 42}"
      fetchData "https://example.com"
    result `shouldBe` Just 42

  it "Handle invalid response" do
    result <- runMockT do
      _makeRequest $ "https://example.com" |> BS.pack "invalid response"
      fetchData "https://example.com"
    result `shouldBe` Nothing
