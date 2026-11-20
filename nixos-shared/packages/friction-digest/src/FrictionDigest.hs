{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}
-- | Verbatim Friction lists from one day's local Claude Code transcripts, by session, sorted by time.
module FrictionDigest
  ( Turn (..), Session (..)
  , decodeTranscript, titleOf, turnOf, ownRun
  , isFriction, frictionLists, parseDataPos
  , session, digest, render
  , transcripts
  , WriteAction (..), writeAction
  ) where

import Control.Lens
import Control.Monad (filterM, forM)
import Data.Aeson (Value, decodeStrict')
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import Data.Containers.ListUtils (nubOrdOn)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Text.Read (readMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Pandoc (Block (..), Extension (Ext_sourcepos), Inline (..), Pandoc (..), def, enableExtension, readerExtensions, runPure)
import Text.Pandoc.Extensions (githubMarkdownExtensions)
import Text.Pandoc.Readers (readCommonMark)

makePrisms ''Block

-- Domain --------------------------------------------------------------------

data Turn = Turn {ts :: UTCTime, cwd :: Text, texts :: [Text]}

data Session = Session
  { sid :: Text
  , title :: Maybe Text
  , sCwd :: Text
  , lists :: NonEmpty (LocalTime, Text) -- distinct Friction lists, verbatim, first-seen order
  }

-- Decoding: folds over Value, so a missing or retyped field yields nothing ----

titleOf :: Fold Value Text
titleOf = key "aiTitle" . _String

turnOf :: Fold Value Turn
turnOf = filtered isMainAssistant . to mkTurn . _Just
  where
    isMainAssistant v =
      v ^? key "type" . _String == Just "assistant"
        && hasn't (key "isSidechain" . _Bool . only True) v -- absent counts as main thread
    mkTurn v =
      Turn
        <$> v ^? key "timestamp" . _JSON
        <*> pure (fromMaybe "?" (v ^? key "cwd" . _String))
        <*> pure
          ( v ^.. key "message" . key "content" . values
              . filtered (has (key "type" . _String . only "text"))
              . key "text" . _String
          )

-- Headless runs (scheduled agents, -p) are "sdk-cli". No transcript carried the
-- --name of a scheduled agent when checked (2026-11-20, p1g8).
ownRun :: [Value] -> Bool
ownRun = has (folded . key "entrypoint" . _String . only "sdk-cli")

-- Parsing: pandoc finds the lists, the source text is what's kept -------------

isFriction :: Block -> Bool
isFriction = \case
  Para [Strong [Str "Friction"]] -> True -- FRIC/list
  Header _ _ [Str "Friction"] -> True -- /mh:retro
  _ -> False -- inline mentions, e.g. the rule text itself

-- sourcepos wraps each top-level block in one Div, except blocks with their own
-- Attr, which carry the position there. A list's own end overruns into the next
-- block; its last item's end does not, so that is where a slice stops.
posOf :: Block -> Maybe Text
posOf b = b ^? attr . _3 . folded . filtered ((== "data-pos") . fst) . _2
  where
    attr = _Div . _1 `failing` _Header . _2 `failing` _CodeBlock . _1 `failing` _Table . _1 `failing` _Figure . _1

-- | @"L:C-L:C;L:C-L:C"@ to the first and last line it covers. Ends are
-- exclusive: an end at column 1 means the range stopped on the line before.
parseDataPos :: Text -> Maybe (Int, Int)
parseDataPos p = do
  let ranges = T.splitOn ";" p
  firstRange <- ranges ^? _head
  lastRange <- ranges ^? _last
  (sl, _) <- lineCol (fst (T.breakOn "-" firstRange))
  (el, ec) <- lineCol (T.drop 1 (snd (T.breakOn "-" lastRange)))
  pure (sl, if ec == 1 then el - 1 else el)
  where
    lineCol :: Text -> Maybe (Int, Int)
    lineCol x = case T.splitOn ":" x of
      [l, c] -> (,) <$> readMaybe (T.unpack l) <*> readMaybe (T.unpack c)
      _ -> Nothing

frictionLists :: Text -> [Text]
frictionLists t
  | not ("Friction" `T.isInfixOf` t) = [] -- prefilter: pandoc never sees most blocks
  | otherwise = either (const []) slices $ do
      Pandoc _ plain <- parse githubMarkdownExtensions
      Pandoc _ wrapped <- parse (enableExtension Ext_sourcepos githubMarkdownExtensions)
      pure (plain, wrapped)
  where
    parse exts = runPure (readCommonMark def {readerExtensions = exts} t)
    ls = T.lines t
    slices (plain, wrapped)
      | length plain /= length wrapped = [] -- the parses disagree: never guess a slice
      | otherwise = mapMaybe (slice wrapped) (hits plain)
    slice wrapped i = do
      (start, _) <- wrapped ^? ix i >>= posOf >>= parseDataPos
      (_, end) <- wrapped ^? ix (i + 1) . _Div . _2 . _head . _BulletList . _last . _head >>= posOf >>= parseDataPos
      pure (T.stripEnd (T.unlines (take (end - start + 1) (drop (start - 1) ls))))
    hits bs = map fst . filter (\(_, (h, n)) -> isFriction h && has _BulletList n) $ zip [0 :: Int ..] (zip bs (drop 1 bs))

-- Pure core -----------------------------------------------------------------------

session :: TimeZone -> Day -> Text -> [Value] -> Maybe Session
session tz d s vs = Session s (lastOf (folded . titleOf) vs) firstCwd . fmap snd <$> nonEmpty found
  where
    local = utcToLocalTime tz . ts
    onDay = filter ((== d) . localDay . local) (vs ^.. folded . turnOf)
    found = nubOrdOn (snd . snd) (concatMap (\t -> map (\fl -> (cwd t, (local t, fl))) (foldMap frictionLists (texts t))) onDay)
    firstCwd = maybe "?" fst (found ^? _head)

digest :: TimeZone -> Day -> [(Text, [Value])] -> [Session]
digest tz d = sortOn (fst . NE.head . lists) . mapMaybe (uncurry (session tz d)) . filter (not . ownRun . snd)

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

-- | Undecodable lines are dropped: a format change must not fail the hourly run.
decodeTranscript :: B.ByteString -> [Value]
decodeTranscript = mapMaybe decodeStrict' . B.lines

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
