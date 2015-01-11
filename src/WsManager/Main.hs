{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Lens        ((^.))
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Data.Text.IO        (readFile)
import           Prelude             hiding (readFile)
import           System.Directory    (doesFileExist)
import           System.Environment  (getEnv)
import           WsManager.Parser
import           WsManager.Types

{-
 GUI-Anforderungen

   * Funktion, die einen "Seitentitel"

 -}

main :: IO ()
main = do
  home <- getEnv "HOME"
  let urlfile = home <> "/.config/wsbaseurl"
  urlFileExists <- doesFileExist urlfile
  if urlFileExists
  then do
    baseUrl <- T.strip <$> readFile urlfile
--     mapM_ print =<< searchSeries baseUrl getter "the wire"
--     mapM_ print =<< searchSeasons baseUrl getter (SeriesHeader "" "/serie/the_wire")
    ss <- searchSeasons baseUrl getter (SeriesHeader "" "/serie/the_wire")
    mapM_ print =<< searchEpisodeDownloadLinks baseUrl getter (head (head ss ^. seasonEpisodes))
    return ()
  else putStrLn ("Url file " <> urlfile <> "does not exist")

