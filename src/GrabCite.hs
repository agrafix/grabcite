module GrabCite
    ( getCitationsFromPdf, getCitationsFromPdfBs
    , getCitationsFromPlainText, getCitationsFromTextFile
    , getCitationsFromIceCiteJson
    , Cfg(..)
    )
where

import GrabCite.Annotate
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.Pipeline
import GrabCite.Pipeline.IceCite
import GrabCite.Pipeline.PdfToText
import Util.Pdf

import Control.Logger.Simple
import Path
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as T

data Cfg
    = Cfg
    { c_refCache :: !RefCache
    }

getCitationsFromTextFile :: Cfg -> Path t File -> IO (ExtractionResult (Maybe DblpPaper))
getCitationsFromTextFile rc fp =
    do r <- T.readFile (toFilePath fp)
       go rc (pdfToTextAsInput r)

getCitationsFromPdf :: Cfg -> Path t File -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromPdf rc fp =
    do r <- extractTextFromPdf fp
       T.mapM (go rc) (pdfToTextAsInput <$> r)

getCitationsFromPdfBs ::
    Cfg -> BS.ByteString -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromPdfBs rc bs =
    do r <- extractTextFromPdfBs bs
       T.mapM (go rc) (pdfToTextAsInput <$> r)

getCitationsFromPlainText :: Cfg -> T.Text -> IO (ExtractionResult (Maybe DblpPaper))
getCitationsFromPlainText c = go c . pdfToTextAsInput

getCitationsFromIceCiteJson :: Cfg -> BS.ByteString -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromIceCiteJson rc bs =
    do res <-
           case iceCiteJsonAsInput bs of
             Left errMsg ->
                 do logError (T.pack errMsg)
                    pure Nothing
             Right ok -> pure (Just ok)
       T.mapM (go rc) res

go :: Cfg -> Input -> IO (ExtractionResult (Maybe DblpPaper))
go rc txt =
    do let extracted = extractCitations txt
       nodes' <- annotateReferences (c_refCache rc) (er_nodes extracted)
       paperId <-
           getPaperId (c_refCache rc) $
           case txt of
             InStructured (StructuredIn { si_title = Just title }) ->
                 Left title
             _ -> Right (er_nodes extracted)
       pure $ extracted { er_nodes = nodes', er_paperId = paperId }
