module GrabCite
    ( getCitationsFromPdf, getCitationsFromPdfBs
    , getCitationsFromPlainText
    )
where

import GrabCite.Annotate
import GrabCite.Dblp
import GrabCite.GetCitations
import Util.Pdf

import Path
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Traversable as T

getCitationsFromPdf :: Path t File -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromPdf fp =
    do r <- extractTextFromPdf fp
       T.mapM go r

getCitationsFromPdfBs :: BS.ByteString -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromPdfBs bs =
    do r <- extractTextFromPdfBs bs
       T.mapM go r

getCitationsFromPlainText :: T.Text -> IO (ExtractionResult (Maybe DblpPaper))
getCitationsFromPlainText = go

go :: T.Text -> IO (ExtractionResult (Maybe DblpPaper))
go txt =
    do let extracted = extractCitations txt
       nodes' <- annotateReferences (er_nodes extracted)
       pure $ extracted { er_nodes = nodes' }
