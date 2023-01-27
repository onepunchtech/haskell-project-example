module Demo (
    demo,
) where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Function
import Data.Time
import Data.Vector.Storable qualified as V
import Datasources.Alpaca.API
import Datasources.Alpaca.Data
import FFI.Rust
import Text.Pretty.Simple

import Monad

exampleLeak :: Integer
exampleLeak =
    -- memory leak
    -- let xs = [1 .. 1000000 :: Integer]
    --  in sum xs * product xs

    -- fix
    let makeXs n = [1 .. n :: Integer]
     in sum (makeXs 1000000) * product (makeXs 1000000)

demo :: AppM ()
demo = do
    let v = exampleLeak
    liftIO $ print v

-- demo :: AppM ()
-- demo = do
--     now <- liftIO getCurrentTime

--     let stocks = (,V.empty) <$> ["AAPL", "GOOG", "MSFT", "AMZN", "META"]
--         end = addUTCTime (-nominalDay) now
--         start = addUTCTime (-nominalDay * 20) end
--         timeframe = "1Day"

--     env <- ask

--     res <- liftIO $ forConcurrently stocks $ \(symbol, _) -> do
--         run env $ do
--             Bars{bars} <- getBars symbol timeframe start end
--             pure (symbol, barC <$> bars)

--     pPrint res

data StepResult = StepResult
    { errorAcum :: [Int]
    , output :: Double
    }

data Instrument

type Result1 = Double
type Result2 = Double
type Result3 = Double

calc :: Instrument -> Double -> Double
calc _ _ = undefined

calc2 :: Instrument -> Double -> Double
calc2 _ _ = undefined

step1Price :: Instrument -> StepResult -> StepResult
step1Price inst (StepResult err price) = StepResult (1 : err) (calc inst price)

step2Price :: Instrument -> StepResult -> StepResult
step2Price inst (StepResult err price) = StepResult (2 : err) (calc inst price)

step3Price :: Instrument -> StepResult -> StepResult
step3Price inst (StepResult err price) = StepResult err (calc2 inst price)

data Result
    = DidNotConverge (Instrument, Double)
    | Converged Double

handler :: Instrument -> Double -> StepResult
handler inst price =
    let initial = StepResult [] price
        allSteps = step3Price inst . step2Price inst . step1Price inst
     in allSteps initial
