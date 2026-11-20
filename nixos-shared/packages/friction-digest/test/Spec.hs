{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM, forM_)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity (runIdentity)
import Data.List (isSuffixOf, sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import FrictionDigest
import Hedgehog hiding (classify)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Streaming.Prelude as S
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain (testGroup "friction-digest" [units, streaming, golden, properties])

day :: Day
day = fromGregorian 2026 11 19

since0 :: UTCTime
since0 = UTCTime (fromGregorian 2000 1 1) 0

lineOf :: Value -> B.ByteString
lineOf = BL.toStrict . encode

-- | The pure driver: the same 'summarize' the IO driver runs, over a list.
summaryOf :: [B.ByteString] -> Summary
summaryOf = runIdentity . summarize since0 . S.each

sess :: Text -> [Value] -> (Text, Summary)
sess s vs = (s, summaryOf (map lineOf vs))

-- Unit ------------------------------------------------------------------------

units :: TestTree
units =
  testGroup
    "frictionLists"
    [ testCase "bold heading, no blank line, kept verbatim up to the next block" $
        frictionLists "TL;DR: x.\n\n**Friction**\n- `a.md`: one\n  continued\n- two\n\nafter\n"
          @?= ["**Friction**\n- `a.md`: one\n  continued\n- two"]
    , testCase "header form at end of text" $
        frictionLists "x\n\n### Friction\n\n- three\n" @?= ["### Friction\n\n- three"]
    , testCase "inline mention (the rule text itself) is ignored" $
        frictionLists "End with a **Friction** list after any TL;DR.\n- not an item\n" @?= []
    , testCase "heading not followed by a list" $
        frictionLists "**Friction**\n\nNothing to change.\n" @?= []
    , testCase "no Friction at all" $ frictionLists "plain text" @?= []
    , testCase "code block before the list keeps index alignment (regression: !! crash)" $
        frictionLists "```sh\nls\n```\n\n**Friction**\n- one\n" @?= ["**Friction**\n- one"]
    , testCase "two lists in one text" $
        frictionLists "**Friction**\n- a\n\nmid\n\n### Friction\n\n- b\n" @?= ["**Friction**\n- a", "### Friction\n\n- b"]
    , testCase "footnote definition after the list stays out (no block of its own)" $
        frictionLists "**Friction**\n- a\n\n[^1]: note\n" @?= ["**Friction**\n- a"]
    , testCase "a Friction heading inside a code fence is code, not a heading" $
        frictionLists "```markdown\n**Friction**\n- not a list\n```\n" @?= []
    , testCase "parseDataPos: exclusive end at column 1 ends on the line before" $
        parseDataPos "2:3-3:1;3:3-4:1" @?= Just (2, 3)
    , testCase "parseDataPos: ranges out of order (setext heading: underline first)" $
        parseDataPos "2:1-3:1;1:1-3:1" @?= Just (1, 2)
    , testCase "setext heading keeps its text line" $
        frictionLists "Friction\n--------\n\n- a\n" @?= ["Friction\n--------\n\n- a"]
    , testCase "parseDataPos: garbage is Nothing, not a crash" $
        parseDataPos "x:y-z" @?= Nothing
    , testCase "local day boundary: 23:30 UTC is the next day at UTC+1" $
        length (digest (hoursToTimeZone 1) day [mkSessionAt "late" (UTCTime day (23 * 3600 + 1800))]) @?= 0
    , testCase "writeAction: no friction wins over everything" $ writeAction False Nothing "x" [] @?= NoFriction
    , testCase "writeAction: never creates the day dir" $ writeAction False Nothing "x" [someSession] @?= DirMissing
    , testCase "writeAction: unchanged content is not rewritten" $ writeAction True (Just "x") "x" [someSession] @?= Unchanged
    , testCase "writeAction: new or changed content is written" $ do
        writeAction True Nothing "x" [someSession] @?= Write
        writeAction True (Just "old") "x" [someSession] @?= Write
    , testCase "render: cwd outside $HOME stays absolute, missing cwd shows ?" $ do
        let out = render "/home/u" day "h" (digest utc day [sess "s1" [turnJson "/srv/x" 600], sess "s2" [noCwdTurn]])
        assertBool "absolute" ("`/srv/x`" `T.isInfixOf` out)
        assertBool "fallback" ("`?`" `T.isInfixOf` out)
    ]

-- Streaming -------------------------------------------------------------------

streaming :: TestTree
streaming =
  testGroup
    "classify and summarize"
    [ testCase "a turn before since never reaches pandoc" $
        classify (UTCTime day 3600) (lineOf (turnAt "/" (UTCTime day 0) "**Friction**\n- x")) @?= SkipL
    , testCase "a main-thread turn mentioning Friction without a timestamp is drift" $
        classify since0 (lineOf (object ["type" .= ("assistant" :: Text), "message" .= object ["content" .= [textBlock "**Friction**\n- x"]]])) @?= DriftL
    , testCase "sidechain turns are skipped" $
        classify since0 (lineOf (object ["type" .= ("assistant" :: Text), "isSidechain" .= True, "timestamp" .= UTCTime day 0, "message" .= object ["content" .= [textBlock "**Friction**\n- x"]]])) @?= SkipL
    , testCase "a turn that only mentions Friction lists nothing" $
        classify since0 (lineOf (turnAt "/" (UTCTime day 0) "the Friction list was empty")) @?= SkipL
    , testCase "aiTitle records become titles" $
        classify since0 (lineOf (object ["type" .= ("ai-title" :: Text), "aiTitle" .= ("T" :: Text)])) @?= TitleL "T"
    , testCase "unparseable lines are skipped, never fatal" $ classify since0 "{\"Friction\": not json" @?= SkipL
    , testCase "summarize stops reading at a headless record" $
        sumHeadless (runIdentity (summarize since0 (S.each [lineOf headless, error "read past the headless record"]))) @?= True
    , testCase "a headless session yields no digest entry, even with lists before the marker" $
        length (digest utc day [("h", summaryOf [lineOf (turnAt "/" (UTCTime day 0) "**Friction**\n- x"), lineOf headless])]) @?= 0
    , testCase "the IO driver and the pure driver agree on every fixture" $ do
        fs <- fixtureFiles
        forM_ fs $ \f -> do
          io <- summarizeFile since0 f
          pure' <- summaryOf . B.lines <$> B.readFile f
          io @?= pure'
    ]
  where
    headless = object ["type" .= ("user" :: Text), "entrypoint" .= ("sdk-cli" :: Text)]

-- Golden ----------------------------------------------------------------------

fixtureFiles :: IO [FilePath]
fixtureFiles = map ("test/fixtures" </>) . sort . filter (".jsonl" `isSuffixOf`) <$> listDirectory "test/fixtures"

fixtures :: IO [(Text, [B.ByteString])]
fixtures = fixtureFiles >>= \fs -> forM fs (\f -> (,) (T.pack (takeBaseName f)) . B.lines <$> B.readFile f)

golden :: TestTree
golden =
  goldenVsString "fixtures render" "test/golden/fixtures.md" $
    BL.fromStrict . TE.encodeUtf8 . render "/home/u" day "testhost" . digest utc day . map (fmap summaryOf) <$> fixtures

-- Properties ------------------------------------------------------------------

-- Lowercase only, so generated prose can never spell "Friction".
genWords :: Gen Text
genWords = T.unwords <$> Gen.list (Range.linear 1 8) (Gen.text (Range.linear 1 8) Gen.lower)

-- Every heading form 'mayHaveHeading' lets through, setext included.
headings :: [Text]
headings = ["**Friction**", "__Friction__", "# Friction", "## Friction", "### Friction", "Friction\n---"]

-- A heading plus a bullet list, with or without a blank line between, items may wrap.
genBlock :: Gen Text
genBlock = do
  h <- Gen.element headings
  sep <- if h `elem` ["**Friction**", "__Friction__"] then Gen.element ["\n", "\n\n"] else pure "\n\n"
  items <- Gen.list (Range.linear 1 4) $ do
    first <- genWords
    cont <- Gen.maybe genWords
    pure ("- " <> first <> maybe "" ("\n  " <>) cont)
  pure (h <> sep <> T.intercalate "\n" items)

genParas :: Gen [Text]
genParas = Gen.list (Range.linear 0 3) genWords

-- Every block shape that might confuse the slicing. Lists use `*` and no indented
-- code: after a `-` list, CommonMark reads both as part of it.
genFiller :: Gen [Text]
genFiller =
  Gen.list (Range.linear 0 4) . Gen.choice $
    [ genWords
    , Gen.element ["## Other", "> quote", "---", "| a |\n|---|\n| 1 |", "```\nx\n```", "<div>x</div>", "* plain\n  * nested", "1. one\n2. two", "[^1]: note", "an inline **Friction** mention"]
    ]

-- Prose with everything a JSON encoder escapes, plus non-ASCII and the marker words.
genNasty :: Gen Text
genNasty = T.concat <$> Gen.list (Range.linear 0 12) (Gen.choice [genWords, Gen.element ["\"", "\\", "\n", "\t", "\x01", "ä", "😀", "</script>", "Friction", "sdk-cli", "aiTitle", " "]])

genRecord :: Gen Value
genRecord = do
  ty <- Gen.element ["assistant", "assistant", "user", "ai-title", "system" :: Text]
  side <- Gen.maybe Gen.bool
  at <- Gen.maybe (UTCTime day . fromIntegral <$> Gen.int (Range.linear 0 86399))
  entry <- Gen.maybe (Gen.element ["cli", "sdk-cli" :: Text])
  ttl <- Gen.maybe genNasty
  texts <- Gen.list (Range.linear 0 3) (Gen.choice [genNasty, (\a b c -> a <> "\n\n" <> b <> "\n\n" <> c) <$> genNasty <*> genBlock <*> genNasty])
  pure . object . catMaybes $
    [ Just ("type" .= ty)
    , ("isSidechain" .=) <$> side
    , ("timestamp" .=) <$> at
    , ("entrypoint" .=) <$> entry
    , ("aiTitle" .=) <$> ttl
    , Just ("cwd" .= ("/home/u" :: Text))
    , Just ("message" .= object ["content" .= map textBlock texts])
    ]

properties :: TestTree
properties =
  testGroup
    "properties"
    [ testProperty "a list between arbitrary blocks comes back exactly" . property $ do
        before <- forAll genFiller
        block <- forAll genBlock
        post <- forAll genFiller
        frictionLists (T.intercalate "\n\n" (before <> [block] <> post) <> "\n") === [block]
    , testProperty "every extracted list is verbatim input and starts with a heading" . property $ do
        doc <- forAll (T.intercalate "\n\n" <$> Gen.list (Range.linear 1 6) (Gen.choice [genWords, genBlock]))
        let out = frictionLists doc
        assert (all (`T.isInfixOf` doc) out)
        assert (all (\l -> any (`T.isPrefixOf` l) headings) out)
    , testProperty "an inline mention before a list never matches" . property $ do
        pre <- forAll genWords
        post <- forAll genWords
        block <- forAll genBlock
        let list = T.unlines (dropWhile (not . ("- " `T.isPrefixOf`)) (T.lines block)) -- the bullets without their heading
        frictionLists (pre <> " **Friction** " <> post <> "\n" <> list) === []
    , testProperty "text without Friction yields nothing" . property $ do
        doc <- forAll (T.intercalate "\n\n" <$> genParas)
        frictionLists doc === []
    , testProperty "the heading pre-check never drops a list" . property $ do
        doc <- forAll (T.intercalate "\n\n" . concat <$> Gen.list (Range.linear 1 4) (Gen.choice [genFiller, pure <$> genBlock]))
        frictionLists doc === frictionListsWith (const True) doc
    , testProperty "the byte tests never change a line's classification" . property $ do
        rec <- forAll genRecord
        since <- forAll (Gen.element [since0, UTCTime day 43200])
        classify since (lineOf rec) === classifyWith (const True) since (lineOf rec)
    , testProperty "contains (memmem) agrees with isInfixOf" . property $ do
        let bytes lo hi = BS.pack <$> Gen.list (Range.linear lo hi) (Gen.element [0, 97, 98, 99])
        needle <- forAll (bytes 1 4)
        hay <- forAll (bytes 0 64)
        contains needle hay === BS.isInfixOf needle hay
    , testProperty "duplicating records changes nothing" . property $ do
        fx <- evalIO fixtures
        dup <- forAll (Gen.list (Range.singleton (length fx)) Gen.bool)
        let doubled = zipWith (\b (s, ls) -> (s, if b then ls <> ls else ls)) dup fx
        rendered doubled === rendered fx
    , testProperty "sessions come out sorted by their first list" . property $ do
        mins <- forAll (Gen.list (Range.linear 0 12) (Gen.int (Range.linear 0 1439)))
        let out = digest utc day (zipWith mkSession [0 :: Int ..] mins)
            firsts = map (fst . NE.head . lists) out
        length out === length mins
        firsts === sort firsts
    ]
  where
    rendered = render "/home/u" day "testhost" . digest utc day . map (fmap summaryOf)

textBlock :: Text -> Value
textBlock t = object ["type" .= ("text" :: Text), "text" .= t]

mkSession :: Int -> Int -> (Text, Summary)
mkSession i m = mkSessionAt (T.pack (show i)) (UTCTime day (fromIntegral (m * 60)))

mkSessionAt :: Text -> UTCTime -> (Text, Summary)
mkSessionAt s at = sess s [turnAt "/home/u" at ("**Friction**\n- item " <> s)]

turnAt :: Text -> UTCTime -> Text -> Value
turnAt cwd' at text =
  object
    [ "type" .= ("assistant" :: Text)
    , "timestamp" .= at
    , "cwd" .= cwd'
    , "message" .= object ["content" .= [textBlock text]]
    ]

turnJson :: Text -> Integer -> Value
turnJson cwd' secs = turnAt cwd' (UTCTime day (fromInteger secs)) "**Friction**\n- x"

noCwdTurn :: Value
noCwdTurn =
  object
    [ "type" .= ("assistant" :: Text)
    , "timestamp" .= UTCTime day 700
    , "message" .= object ["content" .= [textBlock "**Friction**\n- y"]]
    ]

someSession :: Session
someSession = case digest utc day [mkSessionAt "s" (UTCTime day 0)] of
  [s] -> s
  other -> error ("fixture yields " <> show (length other) <> " sessions, expected 1")
