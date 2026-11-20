{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (catch, throwIO)
import Control.Lens (folded, has, hasn't, only)
import Data.Aeson.Lens (key, _String)
import Control.Monad (filterM, forM, forM_, when)
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import FrictionDigest
import Network.HostName (getHostName)
import System.Directory (doesDirectoryExist, getHomeDirectory, getModificationTime, removeFile, renameFile)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (takeBaseName, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

usage :: String
usage =
  unlines
    [ "usage: friction-digest [--date YYYY-MM-DD]  print one day's digest (default: yesterday)"
    , "       friction-digest --write              (over)write today's and yesterday's"
    , "                                            ~/Stuff/YYYY-MM/DD-<host>/friction-digest.md"
    ]

main :: IO ()
main = do
  tz <- getCurrentTimeZone
  today <- localDay . utcToLocalTime tz <$> getCurrentTime
  home <- getHomeDirectory
  -- Same host tag as stuff-today: `nixos-p1` -> `p1`.
  host <- (\h -> maybe h id (T.stripPrefix "nixos-" h)) . T.pack <$> getHostName
  let yesterday = addDays (-1) today
      printDay d = BS.putStr . TE.encodeUtf8 . render (T.pack home) d host . digest tz d =<< loadSince tz home d
  args <- getArgs
  case args of
    [] -> printDay yesterday
    ["--date", s] -> maybe (die usage) printDay (parseTimeM False defaultTimeLocale "%F" s)
    ["--write"] -> do
      sessions <- loadSince tz home yesterday -- one load serves both days
      forM_ [yesterday, today] $ \d -> writeDay home host d (digest tz d sessions)
    _ -> die usage

loadSince :: TimeZone -> FilePath -> Day -> IO [(Text, [Value])]
loadSince tz home d = do
  files <- filterM (fmap ((>= d) . localDay . utcToLocalTime tz) . getModificationTime) =<< transcripts (home </> ".claude/projects")
  forM files $ \f -> do
    vs <- decodeTranscript <$> B.readFile f
    -- Format drift: assistant records exist but none decodes. A session with no
    -- assistant reply yet (running, aborted) is not drift.
    when (has (folded . key "type" . _String . only "assistant") vs && hasn't (folded . turnOf) vs) $
      hPutStrLn stderr ("warn: assistant records present but none decoded in " <> f)
    pure (T.pack (takeBaseName f), vs)

-- Bytes, not Data.Text.IO: the output must not depend on the unit's locale.
writeDay :: FilePath -> Text -> Day -> [Session] -> IO ()
writeDay home host d ss = do
  let dir = home </> "Stuff" </> formatTime defaultTimeLocale "%Y-%m" d </> formatTime defaultTimeLocale "%d-" d <> T.unpack host
      out = dir </> "friction-digest.md"
      new = render (T.pack home) d host ss
  dirExists <- doesDirectoryExist dir
  old <- (Just . TE.decodeUtf8Lenient <$> BS.readFile out) `catch` \e -> if isDoesNotExistError e then pure Nothing else throwIO e
  case writeAction dirExists old new ss of
    NoFriction -> pure ()
    Unchanged -> pure ()
    DirMissing -> hPutStrLn stderr ("warn: " <> dir <> " missing, not created (stuff-today's job)")
    Write -> do
      -- Temp file in the same dir, then rename: Syncthing never sees half a digest.
      let tmp = out <> ".tmp"
      BS.writeFile tmp (TE.encodeUtf8 new) `catch` \e -> (removeFile tmp `catch` \(_ :: IOError) -> pure ()) >> throwIO (e :: IOError)
      renameFile tmp out
      putStrLn ("wrote " <> out)
