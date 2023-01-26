module Datasources.Alpaca (
    getQuote,
    getBars,
) where

import Control.Lens
import Data.Aeson qualified as A
import Data.Char
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Time.Format.ISO8601
import Data.Vector qualified as V
import GHC.Generics
import Network.Wreq

data Quote = Quote
    { ap :: Double
    , as :: Double
    , bp :: Double
    , bs :: Double
    , t :: Text
    }
    deriving (Show, Generic)

instance A.FromJSON Quote

newtype Quotes = Quotes
    { quotes :: M.Map Text Quote
    }
    deriving (Show, Generic)

instance A.FromJSON Quotes

data Bar = Bar
    { barT :: Text
    , barO :: Double
    , barH :: Double
    , barL :: Double
    , barC :: Double
    , barV :: Double
    , barN :: Double
    , barVw :: Double
    }
    deriving (Show, Generic, Eq)

barFieldLabelModifier :: String -> String
barFieldLabelModifier field =
    let f = drop 3 field
     in (toLower (head f) : tail f)

instance A.FromJSON Bar where
    parseJSON = A.genericParseJSON (A.defaultOptions{A.fieldLabelModifier = barFieldLabelModifier})

data Bars = Bars
    { bars :: V.Vector Bar
    , symbol :: Text
    , next_page_token :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance A.FromJSON Bars

apiKey :: Text
apiKey = "AKV441H57RMSMYFVHZMM"

-- headers :: --
-- Apca-Api-Key-Id

apiSecret :: Text
apiSecret = "XCVqSayGc55V0q4sdGpfgWKTF7I3VAjQzi1IYkhF"

baseUrl :: Text
baseUrl = "https://data.alpaca.markets"

mkOptions :: Text -> Text -> Options
mkOptions apiKey secret =
    defaults
        & header "Apca-Api-Key-Id" .~ [T.encodeUtf8 apiKey]
        & header "Apca-Api-Secret-Key" .~ [T.encodeUtf8 apiSecret]

getQuote :: Text -> IO Quotes
getQuote sym = do
    let opts = mkOptions apiKey apiSecret
        url = baseUrl <> "/v1beta2/crypto/latest/quotes?symbols=BTC/USD"
    r <- asJSON =<< getWith opts (T.unpack url)
    pure $ r ^. responseBody

getBars :: Text -> Text -> UTCTime -> UTCTime -> IO Bars
getBars symbol timeframe start end = do
    let opts =
            mkOptions apiKey apiSecret
                & param "timeframe" .~ [timeframe]
                & param "start" .~ [T.pack $ iso8601Show start]
                & param "end" .~ [T.pack $ iso8601Show end]
        url = baseUrl <> "/v2/stocks/" <> symbol <> "/bars"

    r <- asJSON =<< getWith opts (T.unpack url)
    pure $ r ^. responseBody
