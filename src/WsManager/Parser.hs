{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module WsManager.Parser where

import           Control.Applicative
import           Control.Lens           ((^.))
import           Control.Monad          ((>=>))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as LBS
import           Data.Char              (chr)
import           Data.List              (nub)
import qualified Data.Map               as Map
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Network.HTTP
import           Network.Stream         (Result)
import           Prelude                hiding (readFile)
import           Safe                   (headMay)
import           Text.HTML.DOM          (parseLBS)
import           Text.Parsec            (parse)
import           Text.Parsec.Char       (anyChar, char, digit, string)
import           Text.Parsec.Combinator (many1)
import           Text.XML               (Document, Element (..), Name,
                                         Node (..))
import           Text.XML.Cursor        (Axis, Cursor, attribute, attributeIs,
                                         content, descendant, element,
                                         fromDocument, node, ($//), (&//))
import WsManager.Types

urlEncodeText :: T.Text -> T.Text
urlEncodeText = T.pack . urlEncode . T.unpack

attributeWith :: Name -> (T.Text -> Bool) -> Axis
attributeWith n f c = case node c of
  NodeElement (Element _ xs _) -> case Map.lookup n xs of
    Just x -> [c | f x]
    _ -> []
  _ -> []

formatLinks :: Cursor -> [SeriesHeader]
formatLinks c = zipWith SeriesHeader (attribute "title" c) (attribute "href" c)

type DocumentGetter = forall m.(MonadIO m,Functor m) => Link -> m Document

searchSeries :: (Functor m,MonadIO m) => Link -> DocumentGetter -> T.Text -> m [SeriesHeader]
searchSeries baseUrl docGetter name = do
  doc <- docGetter (baseUrl <> "/search/" <> urlEncodeText name)
  return (nub (fromDocument doc $// element "a" >=> attributeWith "href" ("/serie/" `T.isPrefixOf`) >=> formatLinks))

parseEpisodeNumber :: T.Text -> Int
parseEpisodeNumber = read . T.unpack

parseEpisodeTitle :: T.Text -> T.Text
parseEpisodeTitle t = case parse (string "Episode " *> many1 digit *> many1 (char (chr 160)) *> many1 anyChar) "" t of
  Left _ -> t
  Right r -> T.pack r

searchEpisode :: Cursor -> Episode
searchEpisode e = Episode number title date link
  where number = parseEpisodeNumber (head (e $// element "meta" >=> attributeIs "itemprop" "episodenumber" >=> attribute "content"))
        title = parseEpisodeTitle (head (e $// element "span" >=> attributeIs "itemprop" "name" &// content))
        date = headMay (e $// element "span" >=> attributeIs "itemprop" "datepublished" &// content)
        link = head (e $// element "a" >=> attribute "href")

-- searchSeason s = s >=> element "li" >=> attributeIs "episode"
searchSeason :: Cursor -> Season
searchSeason s = Season nr (map searchEpisode (s $// (element "li" >=> attributeIs "itemprop" "episode")))
  where nr = (parseSeasonNumber . head) (s $// element "h2" >=> descendant >=> element "span" >=> attributeIs "itemprop" "name" &// content)

parseSeasonNumber :: T.Text -> Int
parseSeasonNumber = read . T.unpack . T.drop 7

searchEpisodeDownloadLink :: Cursor -> EpisodeDownloadLink
searchEpisodeDownloadLink l = EpisodeDownloadLink title link
  where tds :: [Cursor]
        tds = l $// element "td"
        title = head (head tds $// element "span" &// content)
        link = head ((tds !! 1) $// element "a" >=> attribute "href")

searchEpisodeDownloadLinks :: (MonadIO m,Functor m) => Link -> DocumentGetter -> Episode -> m [EpisodeDownloadLink]
searchEpisodeDownloadLinks baseUrl docGetter e = do
  doc <- docGetter (baseUrl <> (e ^. episodeLink))
  return (map searchEpisodeDownloadLink (fromDocument doc $// element "tr" >=> attributeWith "class" ("download_link" `T.isPrefixOf`)))

-- element "foo" :: Cursor -> [Cursor]
-- descendant ::
-- >=> :: (a -> [b]) -> (b -> [c]) -> (a -> [c])
-- $// :: Cursor node -> (Cursor node -> [a]) -> [a]
searchSeasons :: (MonadIO m,Functor m) => Link -> DocumentGetter -> SeriesHeader -> m [Season]
searchSeasons baseUrl docGetter header = do
  doc <- docGetter (baseUrl <> (header ^. showLink))
  return (map searchSeason (fromDocument doc $// element "div" >=> attributeIs "itemprop" "season"))

searchRealDownloadLink :: (MonadIO m,Functor m) => Link -> DocumentGetter -> EpisodeDownloadLink -> m Link
searchRealDownloadLink baseUrl docGetter dll = do
  doc <- docGetter (baseUrl <> (dll ^. episodeDownloadLink))
  return (head (fromDocument doc $// element "a" >=> attributeIs "class" "push_button_blue" >=> attribute "href"))

episodeLinks :: MonadIO m => Link -> m [Link]
episodeLinks = undefined

simpleHttpLift :: (HStream ty,MonadIO m) => Request ty -> m (Result (Response ty))
simpleHttpLift = liftIO . simpleHTTP

getResponseBodyLift :: MonadIO m => Result (Response ty) -> m ty
getResponseBodyLift = liftIO . getResponseBody

httpGet :: (Functor m,MonadIO m) => Link -> m LBS.ByteString
httpGet l = (LBS.fromStrict . B8.pack) <$> ((simpleHttpLift . getRequest . T.unpack) l >>= getResponseBodyLift)

getter :: (Functor m,MonadIO m) => Link -> m Document
getter url = parseLBS <$> httpGet url
