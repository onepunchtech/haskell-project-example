module Config (
    GlobalConfig (..),
) where

import Datasources.Alpaca.Data qualified as Alpaca

newtype GlobalConfig = GlobalConfig
    { alpaca :: Alpaca.Config
    }
