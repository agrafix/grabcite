{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec

import GrabCite
import GrabCite.Annotate
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.IceCite.Types
import GrabCite.Pipeline

import Control.Logger.Simple
import Control.Monad
import Data.Aeson
import Data.Either
import Data.List
import Data.Maybe
import Data.Yaml
import Path
import Path.IO
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Traversable as T
import qualified Data.Vector as V

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
          do let inFiles =
                     flip filter files $ \f ->
                     let ext = fileExtension f
                     in case ext of
                          ".pdf" -> True
                          ".json" -> True
                          _ -> False
                 ymlFiles =
                     filter (\f -> fileExtension f == ".yml") files
             ymlDummy <-
                 mapM (setFileExtension "dummy") ymlFiles
             let findParing inFile =
                     do fn <- setFileExtension "dummy" (filename inFile)
                        let mkPair yml =
                                (,)
                                <$> pure inFile
                                <*> setFileExtension "yml" yml
                        pair <- T.mapM mkPair $ find (\y -> filename y == fn) ymlDummy
                        case pair of
                          Nothing -> fail ("Missing .yml file for " <> toFilePath inFile)
                          Just ok -> pure ok
             parings <-
                 mapM findParing inFiles
             pure $ Seq.fromList parings

goodRefLines :: [T.Text]
goodRefLines =
    [ "M. Collins and Y. Singer. 1999. Unsupervised models"
    ]

sampleMarkerCand :: CitMarkerCand
sampleMarkerCand =
    CitMarkerCand
    { cmc_references = ["Collins and Singer, 1999"]
    , cmc_range = (8544,8569)
    , cmc_markerPair = ('(',')')
    }

sampleInput :: Input
sampleInput =
    InStructured
    StructuredIn
    { si_title = Nothing
    , si_textCorpus = ""
    , si_referenceCandidates = V.fromList goodRefLines
    }

main :: IO ()
main =
    withGlobalLogging (LogConfig Nothing True) $
    withMemRefCache $ \rc ->
    hspec $
    do testCases <- runIO computeTestCases
       jsonFiles <- runIO computeJsonFiles
       describe "get citations" $
           do describe "extract ref names + years" $
                  it "works" $
                  do let years =
                             extractYears "M. Collins and Y. Singer. 1999. Unsupervised models"
                     years `shouldBe` ["1999"]
                     let names =
                             extractRefNames years "M. Collins and Y. Singer. 1999. Unsupervised models"
                     names `shouldBe` ["m collins and y singer","unsupervised models"]
              describe "extract cit info lines" $
                  do it "smoke 1" $
                         length (extractCitInfoLines sampleInput [sampleMarkerCand]) `shouldBe` 1
              describe "bad ref line" $
                  forM_ goodRefLines $ \rl ->
                  it ("should see " <> show rl <> " as good ref line") $
                  isBadRefLine rl `shouldBe` False
       describe "ice cite json parser" $
           forM_ jsonFiles $ \jsonFile ->
           it ("should parse " <> toFilePath jsonFile) $
           do res <- parseIceDocument <$> BS.readFile (toFilePath jsonFile)
              shouldSatisfy res isRight

       let cfg = Cfg { c_refCache = rc }
       describe "citation extractor" $ forM_ testCases $ \(inFile, ymlFile) ->
           do describe ("testcase " <> toFilePath (filename inFile)) $
                  do res <-
                         runIO $
                         do logNote ("Working on " <> showText (toFilePath inFile))
                            case fileExtension inFile of
                                ".pdf" -> getCitationsFromPdf cfg inFile
                                ".json" ->
                                    BS.readFile (toFilePath inFile) >>= getCitationsFromIceCiteJson cfg
                                ext ->
                                    fail ("Unknown input extension: " ++ show ext)
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
