module Main (main) where

import API.HTTP (runServer)
import Analysis.HTTP
import Text.Pretty.Simple

main :: IO ()
main = do
    q <- getQuote BTC
    pPrint q
