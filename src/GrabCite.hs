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

getCitationsFromPdf :: RefCache -> Path t File -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromPdf rc fp =
    do r <- extractTextFromPdf fp
       T.mapM (go rc) r

getCitationsFromPdfBs ::
    RefCache -> BS.ByteString -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromPdfBs rc bs =
    do r <- extractTextFromPdfBs bs
       T.mapM (go rc) r

getCitationsFromPlainText :: RefCache -> T.Text -> IO (ExtractionResult (Maybe DblpPaper))
getCitationsFromPlainText = go

go :: RefCache -> T.Text -> IO (ExtractionResult (Maybe DblpPaper))
go rc txt =
    do let extracted = extractCitations txt
       nodes' <- annotateReferences rc (er_nodes extracted)
       pure $ extracted { er_nodes = nodes' }
