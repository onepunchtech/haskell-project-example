module Main (main) where

import API.HTTP (runServer)
import Analysis.HTTP
import Data.Time
import Text.Pretty.Simple

main :: IO ()
main = do
    now <- getCurrentTime
    let
        end = addUTCTime (-nominalDay) now
        start = addUTCTime (-nominalDay * 20) end
    q <- getBars "AAPL" "1Day" start end
    pPrint q
