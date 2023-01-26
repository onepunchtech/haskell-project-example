module Datasources.Alpaca.Data (
    Quotes (..),
    Quote (..),
    Bars (..),
    Bar (..),
    Config (..),
) where

import Data.Aeson qualified as A
import Data.Char
import Data.Map qualified as M
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Generics

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

data Config = Config
    { keyId :: Text
    , secret :: Text
    }
