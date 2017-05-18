{-# LANGUAGE OverloadedStrings #-}
module GrabCite.Layout where

import GrabCite.Dblp
import GrabCite.GetCitations

import Control.Monad
import Data.Monoid
import Lucid

mainPage :: Html ()
mainPage =
    html_ $
    do head_ $
           title_ "GrabCite :: Reference extraction"
       body_ $
           do h1_ "GrabCite :: Reference Extraction"
              h2_ "Submit a text:"
              form_ [action_ "/submit", method_ "post"] $
                  do textarea_ [rows_ "5", cols_ "5", name_ "text"] ""
                     input_ [type_ "submit", value_ "Extract"]
              h2_ "Or upload a document:"
              form_ [action_ "/submit", method_ "post", enctype_ "multipart/form-data"] $
                  do input_ [type_ "file", name_ "file"]
                     input_ [type_ "submit", value_ "Extract"]

resultsPage :: ExtractionResult (Maybe DblpPaper) -> Html ()
resultsPage er =
    html_ $
    do head_ $
           title_ "GrabCite :: Reference extraction"
       body_ $
           do h1_ "Extraction results"
              h2_ "Annotated Text"
              pre_ $
                  forM_ (er_nodes er) $ \node ->
                  case node of
                    CnText txt -> toHtml txt
                    CnRef ref ->
                        span_ [style_ "margin: 5px; background: yellow;"] $
                        case cr_tag ref of
                          Just paperInfo ->
                              case db_url paperInfo of
                                Just uri ->
                                    a_ [href_ uri] $ toHtml (cr_origMarker ref)
                                Nothing ->
                                    toHtml $ "[DBLP:" <> db_id paperInfo <> "]"
                          Nothing ->
                              toHtml (cr_origMarker ref)
              h2_ "Detected citations (Original Form)"
              ul_ $
                  forM_ (er_citations er) $ \cic ->
                  li_ $ toHtml (cic_line cic)
