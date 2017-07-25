{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Util.Pdf
    ( isValidPDF
    , extractTextFromPdfBs
    , extractTextFromPdf
    )
where

import Path
import System.Exit
import System.IO
import System.Process
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Path.IO as P

_PDF_EOF_ :: BSC.ByteString
_PDF_EOF_ = "%%EOF"
_IMPORTANT_BYTES_ :: Integer
_IMPORTANT_BYTES_ = 2048

isValidPDF :: FilePath -> IO Bool
isValidPDF fp =
    withFile fp ReadMode $ \hdl ->
        do size <- hFileSize hdl
           hSeek hdl AbsoluteSeek $ max 0 $ size - _IMPORTANT_BYTES_
           lastPart <- BS.hGetContents hdl
           let (withoutWS,_) = BSC.spanEnd (`elem` ['\0','\r','\n',' ','\t']) lastPart
           return $ _PDF_EOF_ `BS.isSuffixOf` withoutWS

getPdfToText :: IO (Path Abs File)
getPdfToText =
    do mexe <- P.findExecutable $(mkRelFile "pdftotext")
       case mexe of
           Nothing -> fail "Please install pdftotext first"
           Just fp -> return fp

extractTextFromPdfBs :: BS.ByteString -> IO (Maybe T.Text)
extractTextFromPdfBs pdfData =
    P.withSystemTempFile "pdftextf" $ \fp hdl ->
    do BS.hPut hdl pdfData
       extractTextFromPdf fp

extractTextFromPdf :: Path t File -> IO (Maybe T.Text)
extractTextFromPdf path =
    P.withSystemTempDir "pdftext" $ \dir ->
    do pdfToText <- getPdfToText
       let outFile = $(mkRelFile "out.txt")
           textOut = dir </> outFile
       ok <-
           runCmd pdfToText
           [ "-enc", "UTF-8", "-eol", "unix", "-raw"
           , toFilePath path, toFilePath textOut
           ]
       isThere <- P.doesFileExist textOut
       if ok && isThere
           then Just <$> T.readFile (toFilePath textOut)
           else return Nothing

runCmd :: Path r File -> [String] -> IO Bool
runCmd cmd args =
    do (Nothing, Nothing, Nothing, ph) <-
            createProcess (proc (toFilePath cmd) args)
       ec <- waitForProcess ph
       return $ ec == ExitSuccess
