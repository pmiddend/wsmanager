{-# LANGUAGE TemplateHaskell #-}
module WsManager.Types where

import Control.Lens.TH
import qualified Data.Text as T

type SeriesTitle = T.Text
type Link = T.Text

data SeriesHeader = SeriesHeader {
    _showTitle :: SeriesTitle
  , _showLink  :: Link
  } deriving(Show,Eq)

$(makeLenses ''SeriesHeader)

data Episode = Episode {
    _episodeNumber :: Int
  , _episodeTitle  :: T.Text
  , _episodeDate   :: Maybe T.Text
  , _episodeLink   :: Link
  } deriving(Show)

$(makeLenses ''Episode)

data Season = Season {
    _seasonNumber   :: Int
  , _seasonEpisodes :: [Episode]
  } deriving(Show)

$(makeLenses ''Season)

data EpisodeDownloadLink = EpisodeDownloadLink {
    _episodeDownloadLinkTitle :: T.Text
  , _episodeDownloadLink      :: T.Text
  } deriving(Show)

$(makeLenses ''EpisodeDownloadLink)

