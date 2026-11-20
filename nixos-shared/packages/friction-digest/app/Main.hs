{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (catch, throwIO)
import Control.Monad (filterM, forM, forM_, when)
import qualified Data.ByteString as BS
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
    , "                                            ~/Stuff/YYYY-MM/DD-<host>/autogen-friction-digest.md"
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

-- | Summarizes transcripts touched since local midnight of @d@, one after the
-- other. Transcripts are independent, so @pooledForConcurrently@ (unliftio,
-- -threaded) would parallelize this; measured 2026-11-20 on p1g8 at -N4: half
-- the wall time for 31% more CPU and 16 MiB more memory, a bad trade for an
-- hourly background job.
loadSince :: TimeZone -> FilePath -> Day -> IO [(Text, Summary)]
loadSince tz home d = do
  files <- filterM (fmap ((>= d) . localDay . utcToLocalTime tz) . getModificationTime) =<< transcripts (home </> ".claude/projects")
  forM files $ \f -> do
    s <- summarizeFile (localTimeToUTC tz (LocalTime d midnight)) f
    when (sumDrift s > 0) $
      hPutStrLn stderr ("warn: " <> show (sumDrift s) <> " assistant records mentioning Friction did not decode in " <> f)
    pure (T.pack (takeBaseName f), s)

-- Bytes, not Data.Text.IO: the output must not depend on the unit's locale.
writeDay :: FilePath -> Text -> Day -> [Session] -> IO ()
writeDay home host d ss = do
  let dir = home </> "Stuff" </> formatTime defaultTimeLocale "%Y-%m" d </> formatTime defaultTimeLocale "%d-" d <> T.unpack host
      out = dir </> "autogen-friction-digest.md"
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
