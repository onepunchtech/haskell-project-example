module Datasources.Alpaca.API (
    getQuote,
    getBars,
) where

import Control.Lens
import Control.Monad.Catch
import Data.ByteString.Char8 qualified as C
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Time.Format.ISO8601
import Datasources.Alpaca.Data
import Network.HTTP.Client

import Config
import Monad

newtype Error = ParseErr String
    deriving (Show)

instance Exception Error

baseUrl :: Text
baseUrl = "https://data.alpaca.markets"

getQuote :: Text -> IO Quotes
getQuote sym = do
    undefined

getBars :: (MonadHttp m, MonadThrow m, MonadConfig m) => Text -> Text -> UTCTime -> UTCTime -> m Bars
getBars symbol timeframe start end = do
    GlobalConfig{alpaca} <- getCfg
    let url = baseUrl <> "/v2/stocks/" <> symbol <> "/bars"
    initReq <- parseRequest (T.unpack url)
    let
        req =
            initReq
                { requestHeaders =
                    [ ("Apca-Api-Key-Id", T.encodeUtf8 $ keyId alpaca)
                    , ("Apca-Api-Secret-Key", T.encodeUtf8 $ secret alpaca)
                    ]
                }
                & setQueryString
                    [ ("timeframe", Just $ T.encodeUtf8 timeframe)
                    , ("start", Just $ C.pack $ iso8601Show start)
                    , ("end", Just $ C.pack $ iso8601Show end)
                    ]

    httpJSON req >>= \case
        Left err -> throwM $ ParseErr err
        Right v -> pure v
