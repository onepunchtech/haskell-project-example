module Demo (
    demo,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time
import Data.Vector qualified as V
import Datasources.Alpaca.API
import Datasources.Alpaca.Data
import Text.Pretty.Simple

import Monad

demo :: AppM ()
demo = do
    now <- liftIO getCurrentTime

    let stocks = ["AAPL", "GOOG", "MSFT", "AMZN", "META"]
        end = addUTCTime (-nominalDay) now
        start = addUTCTime (-nominalDay * 20) end
        timeframe = "1Day"

    tAllStocks <- liftIO $ newTVarIO M.empty

    env <- ask

    forM_ stocks $ \symbol -> do
        liftIO $ forkIO $ run env $ do
            Bars{bars} <- getBars symbol timeframe start end
            let closes = barC <$> bars
            liftIO $ atomically $ do
                modifyTVar tAllStocks (M.insert symbol closes)

    wait (length stocks) tAllStocks

wait :: Int -> TVar (M.Map T.Text (V.Vector Double)) -> AppM ()
wait n tAllStocks = do
    stocks <- liftIO $ atomically $ do
        readTVar tAllStocks
            >>= ( \stocks ->
                    if M.size stocks /= n
                        then retry
                        else pure stocks
                )

    pPrint stocks
