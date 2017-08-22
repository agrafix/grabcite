{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec

import GrabCite
import GrabCite.Annotate
import GrabCite.Arxiv
import GrabCite.Dblp
import GrabCite.GetCitations
import GrabCite.IceCite.Types
import GrabCite.Pipeline
import Util.Sentence
import Util.Tex

import Control.Logger.Simple
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Conduit
import Data.Either
import Data.List
import Data.Maybe
import Data.Yaml
import Path
import Path.IO
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
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

testTexDir :: Path Rel Dir
testTexDir = [reldir|test-tex|]

computeTexFiles :: IO (Seq.Seq (Path Abs File))
computeTexFiles =
    walkDirAccum (Just $ \_ _ _ -> pure $ WalkExclude []) ow testTexDir
    where
      ow _ _ files =
          do let jsonFiles =
                     filter (\f -> fileExtension f == ".tex") files
             pure $ Seq.fromList jsonFiles

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
                          ".tex" -> True
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

sampleInput :: StructuredIn
sampleInput =
    StructuredIn
    { si_title = Nothing
    , si_textCorpus = ""
    , si_referenceCandidates = V.fromList goodRefLines
    }

sampleSplitText :: [T.Text]
sampleSplitText =
    [ "There have been a number of approaches to develop general frameworks for derivation: Caron and coworkers <DBLP:http://dblp.org/rec/journals/ita/CaronCM14> abstract over the support for\ncreating derivations, Thiemann  <DBLP:http://dblp.org/rec/conf/wia/Thiemann16> develops criteria for derivable language operators."
    , "Recently, there has been practical interest in the study of\nderivatives and partial derivatives."
    , "Owens and coworkers  <DBLP:http://dblp.org/rec/journals/jfp/OwensRT09> report a functional implementation with some extensions (e.g., character classes) to handle large\ncharacter sets, which is partially rediscovering work on the FIRE\nlibrary  <GC:fasand.firelite.resin.watson.workshop> ."
    , "Might and coworkers\n <DBLP:http://dblp.org/rec/conf/icfp/MightDS11>  <DBLP:http://dblp.org/rec/conf/pldi/0001HM16> push beyond\nregular languages by implementing parsing for\ncontext-free languages using derivatives and demonstrate its\nfeasibility in practice."
    , "Winter and coworkers  <DBLP:http://dblp.org/rec/conf/calco/WinterBR11> study\ncontext-free languages in a coalgebraic setting."
    ]

main :: IO ()
main =
    withGlobalLogging (LogConfig Nothing True) $
    withMemRefCache $ \rc ->
    hspec $
    do testCases <- runIO computeTestCases
       jsonFiles <- runIO computeJsonFiles
       texFiles <- runIO computeTexFiles
       describe "arxiv extractor" $
           do it "loading pipeline works" $
                  do let cfg =
                             ArxivCfg
                             { ac_metaXml = [relfile|test-xml/arxiv-all.xml|]
                             , ac_srcFileDir = [reldir|test-gz|]
                             , ac_desiredSpec = "cs"
                             }
                     outs <-
                         runResourceT $
                         arxivSpecLoadingPipeline cfg $$ CL.consume
                     map as_ident outs `shouldBe` ["0704.0002", "0704.0046"]
              it "parses the xml correctly" $
                  do outs <-
                         runResourceT $
                         parseMetaXml [relfile|test-xml/arxiv-all.xml|] $$ CL.consume
                     outs `shouldBe`
                         [ MetaHeader
                           { mh_ident = "oai:arXiv.org:0704.0002"
                           , mh_datestamp = "2008-12-13"
                           , mh_setSpec = "cs"
                           }
                         , MetaHeader
                           { mh_ident = "oai:arXiv.org:0704.0046"
                           , mh_datestamp = "2009-11-13"
                           , mh_setSpec = "cs"
                           }
                         ]
       describe "sentence splitting" $
           do let splitTest intercal =
                      it ("works for a medium sized example (intercal. by: " <> show intercal <> ")") $
                      do let textBlob = T.intercalate intercal sampleSplitText
                         sentenceSplit textBlob `shouldBe` (map (T.unwords . T.words . T.replace "\n" " ") sampleSplitText)
              splitTest " "
              splitTest "\n \n"
              splitTest " DUmbuasdu. "
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
                         length (extractCitInfoLines (Right sampleInput) [sampleMarkerCand]) `shouldBe` 1
              describe "bad ref line" $
                  forM_ goodRefLines $ \rl ->
                  it ("should see " <> show rl <> " as good ref line") $
                  isBadRefLine rl `shouldBe` False
       describe "ice cite json parser" $
           forM_ jsonFiles $ \jsonFile ->
           it ("should parse " <> toFilePath jsonFile) $
           do res <- parseIceDocument <$> BS.readFile (toFilePath jsonFile)
              shouldSatisfy res isRight
       describe "tex parser" $
           forM_ texFiles $ \texFile ->
           it ("should parse " <> toFilePath texFile) $
           do res <- flip parseTex False <$> T.readFile (toFilePath texFile)
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
                                ".tex" ->
                                    do bblFile <-
                                           setFileExtension "bbl" inFile
                                       x <- BS.readFile (toFilePath inFile)
                                       hasBbl <- doesFileExist bblFile
                                       y <-
                                           if hasBbl
                                           then Just <$> BS.readFile (toFilePath bblFile)
                                           else pure Nothing
                                       getCitationsFromTex cfg x y
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
                         requiredRefs =
                             S.fromList (pi_dblpRefs info)
                         actualRefs = S.fromList dblpRefs
                     it "is parsed" $
                         res `shouldNotBe` Nothing
                     it "has no missing dblp references" $
                         requiredRefs `S.difference` actualRefs `shouldBe` S.empty
                     it "does not have extra (wrong) dblp references" $
                         actualRefs `S.difference` requiredRefs `shouldBe` S.empty
                     it "has correct own dblp" $
                         join (fmap db_url (join (fmap er_paperId res)))
                             `shouldBe` pi_paperId info
                     return ()
