{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module WsManager.Database where

import           Control.Applicative
import           Control.Monad          (filterM, mzero, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON, ToJSON, Value (..), decode,
                                         encode, object, parseJSON, toJSON,
                                         (.:), (.=))
import           Data.Aeson.TH
import           WsManager.Types
import qualified Data.ByteString as BS

data DatabaseEntry = DatabaseEntry {
    _dbShowLink :: Link
  , _dbSeason   :: Int
  , _dbEpisode  :: Int
  }

$(deriveJSON defaultOptions ''DatabaseEntry)

{-
instance FromJSON DatabaseEntry where
  parseJSON (Object v) = DatabaseEntry <$> (v .: "showLink") <*> (v .: "season") <*> (v .: "episode")
  parseJSON _ = mzero

instance ToJSON DatabaseEntry where
  toJSON (DatabaseEntry secs date fs) = object ["showLink" .= secs,"date" .= date,"firstSection" .= fs]
-}

readDatabase :: MonadIO m => m [DatabaseEntry]
readDatabase = readFile >>=

writeDatabase :: MonadIO m => [DatabaseEntry] -> m ()
writeDatabase = undefined
