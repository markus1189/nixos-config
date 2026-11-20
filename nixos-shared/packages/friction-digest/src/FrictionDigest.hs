{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}
-- | Verbatim Friction lists from one day's local Claude Code transcripts, by session, sorted by time.
--
-- Each transcript is one stream: lines → byte test → decode the few survivors →
-- pandoc → fold into a small 'Summary'. All heavy work happens inside that
-- stream, so transcripts are independent and can be summarized in parallel.
module FrictionDigest
  ( Turn (..), Session (..), Summary (..), Line (..)
  , contains, classify, classifyWith, summaryFold, summarize, summarizeFile
  , isFriction, frictionLists, frictionListsWith, mayHaveHeading, parseDataPos
  , session, digest, render
  , transcripts
  , WriteAction (..), writeAction
  ) where

import qualified Control.Foldl as L
import Control.DeepSeq (force)
import Control.Lens hiding (contains, (:>))
import Control.Monad (filterM, forM)
import Data.Aeson (Value, decodeStrict')
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Foreign.C.Types (CSize (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Streaming (Of (..), Stream)
import qualified Streaming.ByteString as Q
import qualified Streaming.ByteString.Char8 as Q8
import qualified Streaming.Prelude as S
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.IO (IOMode (ReadMode), withFile)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Text.Pandoc (Block (..), Extension (Ext_sourcepos), Inline (..), Pandoc (..), def, enableExtension, readerExtensions, runPure)
import Text.Pandoc.Extensions (githubMarkdownExtensions)
import Text.Pandoc.Readers (readCommonMark)
import Text.Pandoc.Walk (walk)
import Text.Read (readMaybe)

makePrisms ''Block

-- Domain --------------------------------------------------------------------

-- | A main-thread assistant turn with at least one Friction list. Strict and
-- forced when built, so pandoc runs inside the transcript's stream and no
-- decoded record outlives its line.
data Turn = Turn {ts :: !UTCTime, cwd :: !Text, found :: ![Text]}
  deriving (Eq, Show)

data Session = Session
  { sid :: Text
  , title :: Maybe Text
  , sCwd :: Text
  , lists :: NonEmpty (LocalTime, Text) -- distinct Friction lists, verbatim, first-seen order
  }

-- | One transcript line, as far as a digest cares.
data Line
  = TitleL !Text
  | TurnL !Turn
  | DriftL -- a main-thread assistant record mentioning Friction that does not decode: format drift
  | HeadlessL -- scheduled agents and -p runs ("sdk-cli"): the whole session is skipped
  | SkipL
  deriving (Eq, Show)

makePrisms ''Line

-- | What one transcript contributes to a digest.
data Summary = Summary
  { sumTitle :: Maybe Text -- the last aiTitle
  , sumTurns :: [Turn] -- in file order
  , sumDrift :: Int
  , sumHeadless :: Bool
  }
  deriving (Eq, Show)

-- Per line: byte test, then decode ---------------------------------------------

foreign import ccall unsafe "memmem"
  c_memmem :: Ptr () -> CSize -> Ptr () -> CSize -> IO (Ptr ())

-- | Substring test via glibc's memmem: 7× faster than 'B.isInfixOf' on real
-- transcripts (26 vs 192 ms for 73 MB, three needles), and the scan is most of
-- a run. Needles are never empty.
contains :: B.ByteString -> B.ByteString -> Bool
contains needle hay = unsafeDupablePerformIO $
  BU.unsafeUseAsCStringLen hay $ \(hp, hl) ->
    BU.unsafeUseAsCStringLen needle $ \(np, nl) ->
      (/= nullPtr) <$> c_memmem (castPtr hp) (fromIntegral hl) (castPtr np) (fromIntegral nl)

-- | Byte tests decide which lines get decoded at all; about 1% pass. Each test
-- is sound: JSON encoders do not escape ASCII letters, so a record whose decoded
-- text contains the word contains it as raw bytes too (a property in the tests).
classify :: UTCTime -> B.ByteString -> Line
classify since l = classifyWith (`contains` l) since l

-- | 'classify' with the byte test as a parameter; @const True@ decodes every line.
-- A record is decoded at most once, however many tests pass. Turns before
-- @since@ are dropped before pandoc sees them.
classifyWith :: (B.ByteString -> Bool) -> UTCTime -> B.ByteString -> Line
classifyWith mayContain since l
  | mayContain "sdk-cli", Just v <- dec, has (key "entrypoint" . _String . only "sdk-cli") v = HeadlessL
  | mayContain "aiTitle", Just t <- dec ^? _Just . key "aiTitle" . _String = TitleL t
  | mayContain "Friction", Just v <- dec, isMainAssistant v, any ("Friction" `T.isInfixOf`) (textsOf v) = case v ^? key "timestamp" . _JSON of
      Nothing -> DriftL
      Just t
        | t < since -> SkipL
        | otherwise -> case force (foldMap frictionLists (textsOf v)) of
            [] -> SkipL -- mentions Friction, lists none
            fls -> TurnL (Turn t (fromMaybe "?" (v ^? key "cwd" . _String)) fls)
  | otherwise = SkipL
  where
    dec = decodeStrict' l :: Maybe Value
    isMainAssistant v =
      v ^? key "type" . _String == Just "assistant"
        && hasn't (key "isSidechain" . _Bool . only True) v -- absent counts as main thread
    textsOf v = v ^.. key "message" . key "content" . values . filtered (has (key "type" . _String . only "text")) . key "text" . _String

-- Per transcript: a fold over classified lines -----------------------------------

summaryFold :: L.Fold Line Summary
summaryFold =
  Summary
    <$> L.handles _TitleL L.last
    <*> L.handles _TurnL L.list
    <*> L.handles _DriftL L.length
    <*> pure False -- 'summarize' sets it: a headless line ends the read

-- | A headless record ends the read: its session is discarded anyway, so the rest
-- of the file is never pulled.
summarize :: Monad m => UTCTime -> Stream (Of B.ByteString) m r -> m Summary
summarize since ls = do
  s :> rest <- L.purely S.fold summaryFold (S.break (has _HeadlessL) (S.map (classify since) ls))
  headless <- either (const False) (const True) <$> S.next rest
  pure s {sumHeadless = headless}

-- | Memory is bounded by the longest line, not the file.
summarizeFile :: UTCTime -> FilePath -> IO Summary
summarizeFile since f = withFile f ReadMode $ \h -> summarize since (S.mapped Q.toStrict (Q8.lines (Q.hGetContents h)))

-- Parsing: pandoc finds the lists, the source text is what's kept -------------

isFriction :: Block -> Bool
isFriction = \case
  Para [Strong [Str "Friction"]] -> True -- FRIC/list
  Header _ _ [Str "Friction"] -> True -- /mh:retro
  _ -> False -- inline mentions, e.g. the rule text itself

-- | Necessary for 'isFriction' to match a top-level block: the heading starts a
-- line. Most texts that mention the word fail it and skip pandoc (17 of 69 on
-- real transcripts; the parse is most of the remaining time).
mayHaveHeading :: Text -> Bool
mayHaveHeading = any heading . T.lines
  where
    heading l =
      let s = T.stripStart l
       in any (`T.isPrefixOf` s) ["**Friction**", "__Friction__"]
            || ("#" `T.isPrefixOf` s && "Friction" `T.isInfixOf` s)
            || T.strip l == "Friction" -- setext

-- | sourcepos wraps each top-level block in a Div and each inline in a Span,
-- except blocks with their own Attr, which carry the position there.
posOf :: Block -> Maybe Text
posOf b = b ^? attr . _3 . folded . filtered ((== "data-pos") . fst) . _2
  where
    attr = _Div . _1 `failing` _Header . _2 `failing` _CodeBlock . _1 `failing` _Table . _1 `failing` _Figure . _1

-- | Drop the sourcepos wrappers, so blocks match as in a plain parse.
unwrap :: Block -> Block
unwrap = \case
  Div (_, _, kv) [b] | wrapper kv -> walk strip b
  b -> walk strip b
  where
    strip = concatMap (\case Span (_, _, kv') is | wrapper kv' -> is; i -> [i])
    wrapper = (== Just "1") . lookup "wrapper"

-- | @"L:C-L:C;L:C-L:C"@ to the first and last line it covers. The ranges are
-- not in order (a setext heading lists its underline first), and ends are
-- exclusive: an end at column 1 means the range stopped on the line before.
parseDataPos :: Text -> Maybe (Int, Int)
parseDataPos p = do
  spans <- traverse range (T.splitOn ";" p)
  (,) <$> minimumOf (folded . _1) spans <*> maximumOf (folded . _2) spans
  where
    range r = do
      (sl, _) <- lineCol (fst (T.breakOn "-" r))
      (el, ec) <- lineCol (T.drop 1 (snd (T.breakOn "-" r)))
      pure (sl, if ec == 1 then el - 1 else el)
    lineCol :: Text -> Maybe (Int, Int)
    lineCol x = case T.splitOn ":" x of
      [l, c] -> (,) <$> readMaybe (T.unpack l) <*> readMaybe (T.unpack c)
      _ -> Nothing

-- | One sourcepos parse: a heading block and the list after it, matched on the
-- unwrapped blocks, sliced from the source by the heading's start and the list's
-- last item's end (the list's own end overruns into the next block).
frictionLists :: Text -> [Text]
frictionLists = frictionListsWith mayHaveHeading

-- | 'frictionLists' with the pre-check as a parameter; @const True@ parses every text.
frictionListsWith :: (Text -> Bool) -> Text -> [Text]
frictionListsWith precheck t
  | not (precheck t) = []
  | otherwise = either (const []) slices (runPure (readCommonMark def {readerExtensions = exts} t))
  where
    exts = enableExtension Ext_sourcepos githubMarkdownExtensions
    ls = T.lines t
    slices (Pandoc _ bs) = let pb = map (\b -> (posOf b, unwrap b)) bs in mapMaybe slice (zip pb (drop 1 pb))
    slice ((hpos, h), (_, l))
      | isFriction h = do
          (start, _) <- hpos >>= parseDataPos
          (_, end) <- l ^? _BulletList . _last . _head >>= posOf >>= parseDataPos
          pure (T.stripEnd (T.unlines (take (end - start + 1) (drop (start - 1) ls))))
      | otherwise = Nothing

-- Pure core -----------------------------------------------------------------------

session :: TimeZone -> Day -> Text -> Summary -> Maybe Session
session tz d s sm
  | sumHeadless sm = Nothing
  | otherwise = Session s (sumTitle sm) firstCwd . fmap snd <$> nonEmpty found'
  where
    local = utcToLocalTime tz . ts
    onDay = filter ((== d) . localDay . local) (sumTurns sm)
    found' = nubOrdOn (snd . snd) (concatMap (\t -> map (\fl -> (cwd t, (local t, fl))) (found t)) onDay)
    firstCwd = maybe "?" fst (found' ^? _head)

digest :: TimeZone -> Day -> [(Text, Summary)] -> [Session]
digest tz d = sortOn (fst . NE.head . lists) . mapMaybe (uncurry (session tz d))

render :: Text -> Day -> Text -> [Session] -> Text
render home d host ss = T.unlines (("# Friction " <> tshow d <> " · " <> host) : "" : concatMap entry ss)
  where
    entry s =
      ["## " <> hhmm (fst (NE.head (lists s))) <> " · " <> fromMaybe (T.take 8 (sid s)) (title s), "`" <> sid s <> "` · `" <> tilde (sCwd s) <> "`", ""]
        <> concat (zipWith listBlock [0 :: Int ..] (NE.toList (lists s)))
    listBlock i (t, fl) = [ "*" <> hhmm t <> "*" | i > 0 ] <> [ "" | i > 0 ] <> [fl, ""] -- later lists carry their own time
    tilde p = maybe p ("~" <>) (T.stripPrefix home p)
    hhmm = T.pack . formatTime defaultTimeLocale "%H:%M"
    tshow :: Show a => a -> Text
    tshow = T.pack . show

-- Edges ---------------------------------------------------------------------------

transcripts :: FilePath -> IO [FilePath] -- top level only: subagents never list (FRIC/list)
transcripts root = do
  exists <- doesDirectoryExist root -- a host or user that never ran Claude Code
  projects <- if exists then filterM doesDirectoryExist . map (root </>) =<< listDirectory root else pure []
  concat <$> forM projects (\p -> map (p </>) . filter ((== ".jsonl") . takeExtension) <$> listDirectory p)

-- | What --write does for one day, decided purely so it can be tested.
data WriteAction = NoFriction | DirMissing | Unchanged | Write
  deriving (Eq, Show)

writeAction :: Bool -> Maybe Text -> Text -> [Session] -> WriteAction
writeAction dirExists old new ss
  | null ss = NoFriction
  | not dirExists = DirMissing
  | old == Just new = Unchanged
  | otherwise = Write
