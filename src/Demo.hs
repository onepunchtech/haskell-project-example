module Demo (
    demo,
) where

import Control.Monad.IO.Class

import Data.Time
import Datasources.Alpaca.API
import Monad
import Text.Pretty.Simple

demo :: AppM ()
demo = do
    now <- liftIO $ getCurrentTime
    let
        end = addUTCTime (-nominalDay) now
        start = addUTCTime (-nominalDay * 20) end
    q <- getBars "AAPL" "1Day" start end
    pPrint q
