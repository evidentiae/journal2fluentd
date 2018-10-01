-- A simple daemon that tails journald events, does some simple processing (see
-- the fieldMap function), formats the events as JSON and then sends them to
-- fluentd

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.List ( find )
import Data.Maybe
import Data.Ratio ( (%) )
import Data.Text.Encoding
import Data.Time.Clock.POSIX
import Data.Time.ISO8601
import GHC.Generics
import Network.Socket
import Pipes
import Pipes.Aeson
import Pipes.Network.TCP ( toSocket )
import Pipes.Safe
import System.Environment
import Systemd.Journal

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Pipes.Prelude as P
import qualified System.Posix.Files

data SType = TCP | UDP deriving (Eq, Generic, A.FromJSON)

data Config = Config
  { cursorFile :: FilePath
  , fluentdHost :: String
  , fluentdPort :: Int
  , socketType :: Maybe SType
  , beginFromStart :: Maybe Bool
  } deriving (Generic, A.FromJSON)

loadConfig :: IO Config
loadConfig = do
  configPath <- getEnv "JOURNAL2FLUENTD_CONFIG"
  json <- BS.readFile configPath
  case A.eitherDecodeStrict json of
    Left err -> error err
    Right config -> pure config

main :: IO ()
main = do
  cfg@Config {cursorFile, beginFromStart} <- loadConfig
  o <- out cfg
  hasCursor <- System.Posix.Files.fileExist cursorFile
  start <- if hasCursor
           then fmap (flip FromCursor Forwards) (BS.readFile cursorFile)
           else case beginFromStart of
                  Just True -> return FromStart
                  _ -> return (FromEnd Forwards)
  runSafeT $ runEffect $ src start >-> P.chain (checkpoint cursorFile) >-> encode >-> o

out :: Config -> IO (Consumer BS.ByteString (SafeT IO) ())
out (Config {fluentdHost, fluentdPort, socketType}) = do
  addrInfo <- getAddrInfo Nothing (Just fluentdHost) (Just (show fluentdPort))
  let serverAddr = head addrInfo
      stype = if socketType == (Just UDP) then Datagram else Stream
  sock <- socket (addrFamily serverAddr) stype defaultProtocol
  connect sock (addrAddress serverAddr)
  return $ toSocket sock

src :: Start -> Producer JournalEntry (SafeT IO) ()
src start = openJournal [] start Nothing Nothing

encode :: Pipe JournalEntry BS.ByteString (SafeT IO) ()
encode = for (P.map tojson) $ \e ->
           for (encodeObject e) $ \bs -> yield bs >> yield "\n"

tojson :: JournalEntry -> HM.HashMap T.Text A.Value
tojson je = HM.fromList $ timestamp : cursor : ident : map fieldMap kvs
  where
    lookupTag :: T.Text -> Maybe T.Text
    lookupTag t = snd <$> find ((== t) . fst) kvs
    stripSuffix' s t = fromMaybe t (T.stripSuffix s t)
    safeUtf8 (decodeUtf8' -> Right t) = t
    safeUtf8 _ = "[DATA]"
    kvs = [(journalField k, safeUtf8 v) | (k,v) <- HM.toList (journalEntryFields je)]
    cursor = ("cursor", A.String $ safeUtf8 (journalEntryCursor je))
    timestamp = ("@timestamp"
                , A.String $ T.pack $ formatISO8601 $
                  posixSecondsToUTCTime $ fromRational $ us % 1000000)
    identText = stripSuffix' ".service" $ head $ catMaybes $
                  map lookupTag ["UNIT","_SYSTEMD_UNIT","SYSLOG_IDENTIFIER"] ++
                  [Just "unknown"]
    ident = ("ident", A.String identText)
    us = case lookupTag "_SOURCE_REALTIME_TIMESTAMP" of
           Just v -> read (T.unpack v)
           Nothing -> toInteger $ journalEntryRealtime je

fieldMap :: (T.Text, T.Text) -> (T.Text, A.Value)
fieldMap ("MESSAGE", v) = ("message", A.String v)
fieldMap ("_HOSTNAME", v) = ("host", A.String $ T.takeWhile ((/=) '.') v)
fieldMap (k, v) = (T.toLower . T.dropWhile ((==) '_') $ k, A.String v)

checkpoint :: String -> JournalEntry -> (SafeT IO) ()
checkpoint cursorFile entry = liftIO $ do
  let tmpCursorFile = cursorFile ++ ".tmp"
  BS.writeFile tmpCursorFile (journalEntryCursor entry)
  System.Posix.Files.rename tmpCursorFile cursorFile
