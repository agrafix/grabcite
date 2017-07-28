{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec

import GrabCite
import GrabCite.Annotate
import GrabCite.Dblp
import GrabCite.GetCitations

import Control.Monad
import Data.Aeson
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Yaml
import Path
import Path.IO
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Traversable as T

data PaperInfo
    = PaperInfo
    { pi_dblpRefs :: ![T.Text]
    } deriving (Show, Eq)

instance FromJSON PaperInfo where
    parseJSON =
        withObject "PaperInfo" $ \o ->
        PaperInfo
        <$> o .: "dblp-refs"

testDataDir :: Path Rel Dir
testDataDir = [reldir|test-data|]

computeTestCases :: IO (Seq.Seq (Path Abs File, Path Abs File))
computeTestCases =
    walkDirAccum (Just $ \_ _ _ -> pure $ WalkExclude []) ow testDataDir
    where
      ow _ _ files =
          do let pdfFiles =
                     filter (\f -> fileExtension f == ".pdf") files
                 ymlFiles =
                     filter (\f -> fileExtension f == ".yml") files
             ymlDummy <-
                 mapM (setFileExtension "dummy") ymlFiles
             let findParing pdfFile =
                     do fn <- setFileExtension "dummy" (filename pdfFile)
                        let mkPair yml =
                                (,)
                                <$> pure pdfFile
                                <*> setFileExtension "yml" yml
                        pair <- T.mapM mkPair $ find (\y -> filename y == fn) ymlDummy
                        case pair of
                          Nothing -> fail ("Missing .yml file for " <> toFilePath pdfFile)
                          Just ok -> pure ok
             parings <-
                 mapM findParing pdfFiles
             pure $ Seq.fromList parings

main :: IO ()
main =
    withMemRefCache $ \rc ->
    hspec $
    do testCases <- runIO computeTestCases
       let cfg = Cfg { c_refCache = rc, c_preNodeSplit = id }
       forM_ testCases $ \(pdfFile, ymlFile) ->
           do describe ("testcase " <> toFilePath (filename pdfFile)) $
                  do res <- runIO (getCitationsFromPdf cfg pdfFile)
                     info <-
                         runIO $
                         do r <- decodeFileEither (toFilePath ymlFile)
                            case r of
                              Left errMsg -> fail (prettyPrintParseException errMsg)
                              Right ok -> pure ok
                     let dblpRefs =
                             flip mapMaybe (maybe [] er_nodes res) $ \cn ->
                             case cn of
                               CnText _ -> Nothing
                               CnRef r ->
                                   join $ db_url <$> cr_tag r
                     it "is parsed" $
                         res `shouldNotBe` Nothing
                     it "has the correct dblp references" $
                         S.fromList (pi_dblpRefs info) `shouldBe` S.fromList dblpRefs
                     return ()
