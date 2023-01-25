module API.Data (
    User (..),
    CreateUserReq (..),
    SuccessMsg (..),
) where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

data User = User
    { name :: Text
    , email :: Text
    }

instance ToJSON User where
    toJSON (User name email) = object ["name" .= name, "email" .= email]

data CreateUserReq = CreateUserReq
    { curName :: Text
    , curEmail :: Text
    }
    deriving (Show, Generic)

instance FromJSON CreateUserReq where
    parseJSON (Object v) =
        CreateUserReq
            <$> (v .: "name")
            <*> (v .: "email")
    parseJSON _ = mzero

newtype SuccessMsg = SuccessMsg {msg :: Text}
    deriving (Show, Generic)

instance ToJSON SuccessMsg
