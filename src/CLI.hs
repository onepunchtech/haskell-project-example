module CLI (runCLI) where

import Config
import Data.Text (Text)
import Datasources.Alpaca.Data qualified as Alpaca
import Demo
import Monad
import Options.Applicative

data Opts = Opts
    { alpacaKeyId :: Text
    , alpacaSecret :: Text
    }

pOpts :: Parser Opts
pOpts =
    Opts
        <$> strOption
            ( long "alpaca-key-id"
                <> metavar "TARGET"
                <> help "alpaca key id"
            )
        <*> strOption
            ( long "alpaca-secret-key"
                <> metavar "TARGET"
                <> help "alpaca secret key"
            )

runCLI :: IO ()
runCLI = runMain =<< execParser opts
  where
    opts =
        info
            (pOpts <**> helper)
            ( fullDesc
                <> progDesc "Fetch some stock data"
                <> header "example - fetching stock data"
            )

runMain :: Opts -> IO ()
runMain Opts{alpacaKeyId, alpacaSecret} = do
    let cfg =
            GlobalConfig
                { alpaca =
                    Alpaca.Config
                        { Alpaca.keyId = alpacaKeyId
                        , Alpaca.secret = alpacaSecret
                        }
                }
    env <- prodEnv cfg
    run env demo
