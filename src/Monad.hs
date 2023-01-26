module Monad (
    AppM,
    MonadHttp (..),
    MonadLog (..),
    MonadConfig (..),
    prodEnv,
    httpJSON,
    run,
) where

import Config
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text
import Data.Text qualified as T
import Network.HTTP.Client qualified as C
import Network.HTTP.Client.TLS

data Env = Env
    { _log :: Text -> IO ()
    , _manager :: C.Manager
    , _httpLBS :: C.Request -> C.Manager -> IO (C.Response LBS.ByteString)
    , _config :: GlobalConfig
    }

class HasLog env where
    getLog :: env -> Text -> IO ()

instance HasLog Env where
    getLog = _log

class HasConfig env where
    getConfig :: env -> GlobalConfig

instance HasConfig Env where
    getConfig = _config

class HasManager env where
    getManager :: env -> C.Manager

instance HasManager Env where
    getManager = _manager

class HasHttpLBS env where
    getHttpLBS :: env -> C.Request -> C.Manager -> IO (C.Response LBS.ByteString)

instance HasHttpLBS Env where
    getHttpLBS = _httpLBS

class MonadHttp m where
    httpLbs :: C.Request -> m (C.Response LBS.ByteString)

class MonadLog m where
    log :: Text -> m ()

class MonadConfig m where
    getCfg :: m GlobalConfig

instance (HasConfig env, Monad m) => MonadConfig (ReaderT env m) where
    getCfg = asks getConfig

instance (HasManager env, HasHttpLBS env, MonadIO m) => MonadHttp (ReaderT env m) where
    httpLbs req = do
        env <- ask
        let manager = getManager env
            call = getHttpLBS env
        liftIO $ call req manager

httpJSON :: (MonadHttp m, MonadThrow m, FromJSON a) => C.Request -> m (Either String a)
httpJSON req = do
    res <- httpLbs req
    pure $ eitherDecode (C.responseBody res)

type AppM = ReaderT Env IO

prodEnv :: GlobalConfig -> IO Env
prodEnv cfg = do
    mgr <- newTlsManager
    pure
        Env
            { _log = putStrLn . T.unpack
            , _manager = mgr
            , _httpLBS = C.httpLbs
            , _config = cfg
            }

run :: Env -> AppM a -> IO a
run env app = runReaderT app env
