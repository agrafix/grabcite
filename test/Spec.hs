{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec

import GrabCite
import GrabCite.Annotate
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.IceCite.Types

import Control.Monad
import Data.Aeson
import Data.Either
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Yaml
import Path
import Path.IO
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Traversable as T

data PaperInfo
    = PaperInfo
    { pi_paperId :: !(Maybe T.Text)
    , pi_dblpRefs :: ![T.Text]
    } deriving (Show, Eq)

instance FromJSON PaperInfo where
    parseJSON =
        withObject "PaperInfo" $ \o ->
        PaperInfo
        <$> o .:? "my-dblp"
        <*> o .: "dblp-refs"

testDataDir :: Path Rel Dir
testDataDir = [reldir|test-data|]

testJsonDir :: Path Rel Dir
testJsonDir = [reldir|test-json|]

computeJsonFiles :: IO (Seq.Seq (Path Abs File))
computeJsonFiles =
    walkDirAccum (Just $ \_ _ _ -> pure $ WalkExclude []) ow testJsonDir
    where
      ow _ _ files =
          do let jsonFiles =
                     filter (\f -> fileExtension f == ".json") files
             pure $ Seq.fromList jsonFiles

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
       jsonFiles <- runIO computeJsonFiles

       describe "ice cite json parser" $
           forM_ jsonFiles $ \jsonFile ->
           it ("should parse " <> toFilePath jsonFile) $
           do res <- parseIceDocument <$> BS.readFile (toFilePath jsonFile)
              shouldSatisfy res isRight

       let cfg = Cfg { c_refCache = rc }
       describe "citation extractor" $ forM_ testCases $ \(pdfFile, ymlFile) ->
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
                         S.fromList dblpRefs `shouldBe` S.fromList (pi_dblpRefs info)
                     it "has correct own dblp" $
                         join (fmap db_url (join (fmap er_paperId res)))
                             `shouldBe` pi_paperId info
                     return ()
