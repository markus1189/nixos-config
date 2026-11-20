{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.List (isSuffixOf, sort)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import FrictionDigest
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, (</>))
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain (testGroup "friction-digest" [units, golden, properties])

day :: Day
day = fromGregorian 2026 11 19

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
    , testCase "parseDataPos: exclusive end at column 1 ends on the line before" $
        parseDataPos "2:3-3:1;3:3-4:1" @?= Just (2, 3)
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
        let out = render "/home/u" day "h" (digest utc day [("s1", [turnJson "/srv/x" 600]), ("s2", [noCwdTurn])])
        assertBool "absolute" ("`/srv/x`" `T.isInfixOf` out)
        assertBool "fallback" ("`?`" `T.isInfixOf` out)
    , testCase "decodeTranscript drops undecodable lines" $
        length (decodeTranscript "{\"a\":1}\nnot json\n{\"b\":2}\n") @?= 2
    ]

-- Golden ----------------------------------------------------------------------

fixtures :: IO [(Text, [Value])]
fixtures = do
  fs <- sort . filter (".jsonl" `isSuffixOf`) <$> listDirectory dir
  forM fs $ \f -> (,) (T.pack (takeBaseName f)) . decodeTranscript <$> B.readFile (dir </> f)
  where
    dir = "test/fixtures"

golden :: TestTree
golden =
  goldenVsString "fixtures render" "test/golden/fixtures.md" $
    BL.fromStrict . TE.encodeUtf8 . render "/home/u" day "testhost" . digest utc day <$> fixtures

-- Properties ------------------------------------------------------------------

-- Lowercase only, so generated prose can never spell "Friction".
genWords :: Gen Text
genWords = T.unwords <$> Gen.list (Range.linear 1 8) (Gen.text (Range.linear 1 8) Gen.lower)

genHeading :: Gen Text
genHeading = Gen.element ["**Friction**", "### Friction", "## Friction"]

-- A heading plus a bullet list, with or without a blank line between, items may wrap.
genBlock :: Gen Text
genBlock = do
  h <- genHeading
  sep <- if h == "**Friction**" then Gen.element ["\n", "\n\n"] else pure "\n\n"
  items <- Gen.list (Range.linear 1 4) $ do
    first <- genWords
    cont <- Gen.maybe genWords
    pure ("- " <> first <> maybe "" ("\n  " <>) cont)
  pure (h <> sep <> T.intercalate "\n" items)

genParas :: Gen [Text]
genParas = Gen.list (Range.linear 0 3) genWords

-- Every block shape that might break the plain/sourcepos index alignment. Lists use
-- `*` and no indented code: after a `-` list, CommonMark reads both as part of it.
genFiller :: Gen [Text]
genFiller =
  Gen.list (Range.linear 0 4) . Gen.choice $
    [ genWords
    , Gen.element ["## Other", "> quote", "---", "| a |\n|---|\n| 1 |", "```\nx\n```", "<div>x</div>", "* plain\n  * nested", "1. one\n2. two", "[^1]: note"]
    ]

properties :: TestTree
properties =
  testGroup
    "properties"
    [ testProperty "a list between arbitrary paragraphs comes back exactly" . property $ do
        before <- forAll genFiller
        block <- forAll genBlock
        post <- forAll genFiller
        frictionLists (T.intercalate "\n\n" (before <> [block] <> post) <> "\n") === [block]
    , testProperty "every extracted list is verbatim input and starts with a heading" . property $ do
        doc <- forAll (T.intercalate "\n\n" <$> Gen.list (Range.linear 1 6) (Gen.choice [genWords, genBlock]))
        let out = frictionLists doc
        assert (all (`T.isInfixOf` doc) out)
        assert (all (\l -> any (`T.isPrefixOf` l) ["**Friction**", "### Friction", "## Friction"]) out)
    , testProperty "an inline mention before a list never matches" . property $ do
        pre <- forAll genWords
        post <- forAll genWords
        block <- forAll genBlock
        let list = T.unlines (drop 1 (T.lines block)) -- the bullets without their heading
        frictionLists (pre <> " **Friction** " <> post <> "\n" <> list) === []
    , testProperty "text without Friction yields nothing" . property $ do
        doc <- forAll (T.intercalate "\n\n" <$> genParas)
        frictionLists doc === []
    , testProperty "duplicating records changes nothing" . property $ do
        sessions <- evalIO fixtures
        dup <- forAll (Gen.list (Range.singleton (length sessions)) Gen.bool)
        let doubled = zipWith (\b (s, vs) -> (s, if b then vs <> vs else vs)) dup sessions
        rendered doubled === rendered sessions
    , testProperty "sessions come out sorted by their first list" . property $ do
        mins <- forAll (Gen.list (Range.linear 0 12) (Gen.int (Range.linear 0 1439)))
        let out = digest utc day (zipWith mkSession [0 :: Int ..] mins)
            firsts = map (fst . NE.head . lists) out
        length out === length mins
        firsts === sort firsts
    ]
  where
    rendered = render "/home/u" day "testhost" . digest utc day

mkSession :: Int -> Int -> (Text, [Value])
mkSession i m = mkSessionAt (T.pack (show i)) (UTCTime day (fromIntegral (m * 60)))

mkSessionAt :: Text -> UTCTime -> (Text, [Value])
mkSessionAt s at = (s, [turnAt "/home/u" at ("**Friction**\n- item " <> s)])

turnAt :: Text -> UTCTime -> Text -> Value
turnAt cwd at text =
  object
    [ "type" .= ("assistant" :: Text)
    , "timestamp" .= at
    , "cwd" .= cwd
    , "message" .= object ["content" .= [object ["type" .= ("text" :: Text), "text" .= text]]]
    ]

turnJson :: Text -> Integer -> Value
turnJson cwd secs = turnAt cwd (UTCTime day (fromInteger secs)) "**Friction**\n- x"

noCwdTurn :: Value
noCwdTurn =
  object
    [ "type" .= ("assistant" :: Text)
    , "timestamp" .= UTCTime day 700
    , "message" .= object ["content" .= [object ["type" .= ("text" :: Text), "text" .= ("**Friction**\n- y" :: Text)]]]
    ]

someSession :: Session
someSession = case digest utc day [mkSessionAt "s" (UTCTime day 0)] of
  [s] -> s
  other -> error ("fixture yields " <> show (length other) <> " sessions, expected 1")
