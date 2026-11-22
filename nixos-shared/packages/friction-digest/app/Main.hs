{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (catch, onException, throwIO)
import Control.Monad (filterM, forM, forM_, when)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import FrictionDigest
import Options.Applicative
import System.Directory (doesDirectoryExist, getHomeDirectory, getModificationTime, removeFile, renameFile)
import System.FilePath (takeBaseName, (</>))
import System.IO (hClose, hPutStrLn, openBinaryTempFile, stderr)
import System.IO.Error (isDoesNotExistError)

data Mode = PrintDay (Maybe Day) | WriteDays [Day]

data Opts = Opts Text Mode

opts :: ParserInfo Opts
opts =
  info
    (Opts <$> hostOpt <*> (writeMode <|> printMode) <**> helper)
    (fullDesc <> progDesc "Collect verbatim Friction lists from Claude Code transcripts")
  where
    dateOpt h = option (eitherReader parseDay) (long "date" <> metavar "YYYY-MM-DD" <> help h)
    parseDay s = maybe (Left ("not a YYYY-MM-DD date: " <> s)) Right (parseTimeM False defaultTimeLocale "%F" s)
    printMode = PrintDay <$> optional (dateOpt "print this day's digest (default: yesterday)")
    writeMode =
      flag' WriteDays (long "write" <> help "(over)write ~/Stuff/YYYY-MM/DD-<host>/autogen-friction-digest.md")
        <*> many (dateOpt "with --write: these days instead of yesterday and today (repeatable)")
    hostOpt = T.pack <$> strOption (long "host" <> metavar "TAG" <> help "host tag in DD-<host> (config.my.hostTag)")

main :: IO ()
main = do
  Opts host mode <- execParser opts
  tz <- getCurrentTimeZone
  today <- localDay . utcToLocalTime tz <$> getCurrentTime
  home <- getHomeDirectory
  let yesterday = addDays (-1) today
  case mode of
    PrintDay d -> do
      let day = fromMaybe yesterday d
      BS.putStr . TE.encodeUtf8 . render (T.pack home) day host . digest tz day =<< loadSince tz home day
    WriteDays ds -> do
      let days = if null ds then [yesterday, today] else ds
      sessions <- loadSince tz home (minimum days) -- one load serves every day
      forM_ days $ \d -> writeDay home host d (digest tz d sessions)

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
      -- Unique name: a manual run racing the timer must not share one temp file.
      (tmp, h) <- openBinaryTempFile dir "autogen-friction-digest.tmp"
      (BS.hPut h (TE.encodeUtf8 new) >> hClose h >> renameFile tmp out)
        `onException` (hClose h >> removeFile tmp `catch` \(_ :: IOError) -> pure ())
      putStrLn ("wrote " <> out)
