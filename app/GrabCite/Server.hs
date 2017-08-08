{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Server ( runServer ) where

import GrabCite
import GrabCite.Annotate
import GrabCite.Context
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.GlobalId
import GrabCite.Layout
import GrabCite.Stats

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid

import Control.Monad.Trans
import Path
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

data MySession = EmptySession
type MyAppState = Cfg

type App = SpockM () MySession MyAppState ()
type Action = SpockAction () MySession MyAppState

runServer :: Int -> IO ()
runServer port =
    withMemRefCache $ \rc ->
    do spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (Cfg rc)
       runSpock port (spock spockCfg app)

app :: App
app =
    do get root $ lucid mainPage
       post "submit" $
           do mText <- param "text"
              case mText of
                Nothing ->
                    do mFile <- HM.lookup "file" <$> files
                       case mFile of
                         Just f ->
                             handleFile f
                         Nothing ->
                             text "Missing text or file"
                Just txt -> handleText txt

handleFile :: UploadedFile -> Action ()
handleFile uf =
    do path <- liftIO $ parseAbsFile (uf_tempLocation uf)
       st <- getState
       result <- liftIO $ getCitationsFromPdf st path
       case result of
         Nothing -> text "Invalid PDF"
         Just er -> handleResults er

handleText :: T.Text -> Action ()
handleText txt =
    do st <- getState
       liftIO (getCitationsFromPlainText st txt) >>= handleResults

handleResults :: ExtractionResult (Maybe DblpPaper) -> Action ()
handleResults er =
    do let stats = getCitStats er
           contexts = getContextedMarkers 100 (globalCitId <$> er_nodes er)
       lucid $ resultsPage er stats contexts
