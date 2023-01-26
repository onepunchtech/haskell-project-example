module Analysis.HTTP (
    getQuote,
    Symbol (..),
) where

import Control.Lens
import Data.Aeson qualified as A
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics
import Network.Wreq

data Symbol
    = BTC
    | ETH

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

apiKey :: Text
apiKey = "AKV441H57RMSMYFVHZMM"

-- headers :: --
-- Apca-Api-Key-Id

apiSecret :: Text
apiSecret = "XCVqSayGc55V0q4sdGpfgWKTF7I3VAjQzi1IYkhF"

baseUrl :: Text
baseUrl = "https://data.alpaca.markets"

getQuote :: Symbol -> IO Quotes
getQuote sym = do
    let opts =
            defaults
                & header "Apca-Api-Key-Id" .~ [T.encodeUtf8 apiKey]
                & header "Apca-Api-Secret-Key" .~ [T.encodeUtf8 apiSecret]
        url = baseUrl <> "/v1beta2/crypto/latest/quotes?symbols=BTC/USD"
    r <- asJSON =<< getWith opts (T.unpack url)
    pure $ r ^. responseBody

-- {"quotes":{"BTC/USD":{"ap":22882.25,"as":0.060306,"bp":22873.29,"bs":0.249681,"t":"2023-01-25T20:24:59.794328662Z"}}}
