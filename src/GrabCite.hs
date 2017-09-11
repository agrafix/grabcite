module GrabCite
    ( getCitationsFromPdf, getCitationsFromPdfBs
    , getCitationsFromPlainText, getCitationsFromTextFile
    , getCitationsFromTex
    , getCitationsFromIceCiteJson
    , getCitationsFromIceCiteBasicJson
    , getCitationsFromGrobidXml
    , Cfg(..)
    )
where

import GrabCite.Annotate
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.Pipeline
import GrabCite.Pipeline.Grobid
import GrabCite.Pipeline.IceCite
import GrabCite.Pipeline.IceCiteBasic
import GrabCite.Pipeline.PdfToText
import GrabCite.Pipeline.Tex
import Util.Pdf

import Control.Logger.Simple
import Path
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

getCitationsFromGrobidXml :: Cfg -> BS.ByteString -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromGrobidXml rc bs =
    do res <-
           case grobidXmlAsInput bs of
             Left errMsg ->
                 do logError (T.pack errMsg)
                    pure Nothing
             Right ok -> pure (Just ok)
       T.mapM (go rc) res

getCitationsFromIceCiteJson :: Cfg -> BS.ByteString -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromIceCiteJson rc bs =
    do res <-
           case iceCiteJsonAsInput bs of
             Left errMsg ->
                 do logError (T.pack errMsg)
                    pure Nothing
             Right ok -> pure (Just ok)
       T.mapM (go rc) res

getCitationsFromIceCiteBasicJson ::
    Cfg -> BS.ByteString -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromIceCiteBasicJson rc bs =
    do res <-
           case iceCiteJsonAsBasicInput bs of
             Left errMsg ->
                 do logError (T.pack errMsg)
                    pure Nothing
             Right ok -> pure (Just ok)
       T.mapM (go rc) res

getCitationsFromTex ::
    Cfg
    -> BS.ByteString
    -> Maybe BS.ByteString
    -> IO (Maybe (ExtractionResult (Maybe DblpPaper)))
getCitationsFromTex rc bs mBbl =
    do res <-
           case texAsInput (TexInput (robustDecode bs) (robustDecode <$> mBbl)) of
             Left errMsg ->
                 do logError (T.pack errMsg)
                    pure Nothing
             Right ok -> pure (Just ok)
       T.mapM (go rc) res

robustDecode :: BS.ByteString -> T.Text
robustDecode bs =
    case T.decodeUtf8' bs of
      Left _ -> T.decodeLatin1 bs
      Right ok -> ok

go :: Cfg -> Input -> IO (ExtractionResult (Maybe DblpPaper))
go rc txt =
    do let extracted = extractCitations txt
       nodes' <- annotateReferences (c_refCache rc) (er_nodes extracted)
       let titleLine =
               case txt of
                 InStructured (StructuredIn { si_title = Just title }) ->
                     Left title
                 InCited (CitedIn { ci_title = title }) ->
                     Left title
                 _ -> Right (er_nodes extracted)
       paperId <-
           getPaperId (c_refCache rc) titleLine
       pure $
           extracted
           { er_nodes = nodes'
           , er_titleLine =
                   case titleLine of
                     Left x -> Just x
                     Right _ -> Nothing
           , er_paperId = paperId
           }
