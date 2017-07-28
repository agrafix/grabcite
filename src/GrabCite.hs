module GrabCite
    ( getCitationsFromPdf, getCitationsFromPdfBs
    , getCitationsFromPlainText, getCitationsFromTextFile
    , Cfg(..)
    )
where

import GrabCite.Annotate
import GrabCite.Dblp
import GrabCite.GetCitations
import Util.Pdf
import Util.Text

import Path
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as T

data Cfg
    = Cfg
    { c_refCache :: !RefCache
    , c_preNodeSplit :: !(T.Text -> T.Text)
    }

getCitationsFromTextFile :: Cfg -> Path t File -> IO (ExtractionResult (Maybe DblpPaper))
getCitationsFromTextFile rc fp =
    do r <- T.readFile (toFilePath fp)
       go rc (textRemoveLig r)

getCitationsFromPdf :: Cfg -> Path t File -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromPdf rc fp =
    do r <- extractTextFromPdf fp
       T.mapM (go rc) r

getCitationsFromPdfBs ::
    Cfg -> BS.ByteString -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromPdfBs rc bs =
    do r <- extractTextFromPdfBs bs
       T.mapM (go rc) r

getCitationsFromPlainText :: Cfg -> T.Text -> IO (ExtractionResult (Maybe DblpPaper))
getCitationsFromPlainText c = go c . textRemoveLig

go :: Cfg -> T.Text -> IO (ExtractionResult (Maybe DblpPaper))
go rc txt =
    do let extracted = extractCitations txt (c_preNodeSplit rc)
       nodes' <- annotateReferences (c_refCache rc) (er_nodes extracted)
       paperId <- getPaperId (c_refCache rc) (er_nodes extracted)
       pure $ extracted { er_nodes = nodes', er_paperId = paperId }
