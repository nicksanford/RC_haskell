{-# LANGUAGE OverloadedStrings #-}
module FileManager where
import Tracker
import Utils (shaHash, unhex)
import qualified System.IO as SIO
import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.List as L
import Data.Maybe (isJust, fromJust)
import qualified System.IO as SIO
import qualified System.Directory as Dir
import Data.Foldable (forM_)
import System.Exit (exitSuccess)
import Control.Monad (when)

-- data Tracker = Tracker (PeerId BS.ByteString)
--                        (Announce BS.ByteString)
--                        (PieceLength Integer)
--                        (Pieces [BS.ByteString])
--                        (InfoHash BS.ByteString)
--                        (SingleFileInfo)
--                        (Maybe DirectoryInfo)
--                        (Maybe (Encoding BS.ByteString))
--              deriving (Eq, Show)
-- data TrackerResponse = TrackerResponse (Peers [Peer])
--                                        (Maybe (TrackerId BS.ByteString))
--                                        (Maybe (Warning BS.ByteString))
--                                        (Interval Integer)
--                                        (Maybe (MinInterval Integer))
--                                        (Maybe (Complete BS.ByteString))
--                                        (Maybe (InComplete BS.ByteString)) deriving (Eq, Show)

-- data FileManagerState = FileManagerState (PeerId BS.ByteString)
--                                          (PieceLength Integer)
--                                          (Pieces Integer)
--                                          (BlockSize Integer) -- will set this to 16k
--                                          (DownloadedPieces (M.Map BS.ByteString (M.Map )))

blockSize :: Integer
--blockSize = 2^14 -- 16k
blockSize = 2^14 -- 16k
-- TODO: I am going to start off with only implementing this for a single file, in multifile mode I will need to maintain multiple files on the filesystem, I will deal with that once I have gotten single file downloads & uploads working.

newtype PieceIndex a = PieceIndex a deriving (Eq, Show, Ord)
newtype Begin a = Begin a deriving (Eq, Show, Ord)
newtype RequestLength a = RequestLength a deriving (Eq, Show, Ord)
newtype PieceContent a = PieceContent a deriving (Eq, Show, Ord)
newtype PieceSha a = PieceSha a deriving (Eq, Show, Ord)

data BlockRequest = BlockRequest (PieceIndex Integer) (Begin Integer) (RequestLength Integer) deriving (Eq, Show, Ord)

data PieceResponse = PieceResponse (PieceIndex Integer) (PieceContent BS.ByteString) deriving (Eq, Show, Ord)

data WorkMessage = Work [BlockRequest] deriving (Eq, Show)

data ResponseMessage = Failed [BlockRequest]
                     | Succeeded PieceResponse
                     | Error Peer
                     deriving (Eq, Show)

getRequestList :: Tracker -> [BlockRequest]
getRequestList tracker = do
  let pieces :: [BS.ByteString ]
      pieces = getTrackerPieces tracker
  let pieceLength = getTrackerPieceLength tracker
  let xs = [(b,min blockSize (pieceLength - b)) | b <- takeWhile (<pieceLength) $ iterate (+blockSize) 0]
  let ys = [BlockRequest (PieceIndex p) (Begin b) (RequestLength s)
           | p <- [0..fromIntegral $ (length pieces) - 2], (b, s) <- xs]
  let SingleFileInfo (Name _) (Length totalLength) (MD5Sum _) = getTrackerSingleFileInfo tracker
  let remainingLength = totalLength - pieceLength * (fromIntegral $ length pieces - 1)
  let lastPieceIndex = fromIntegral (length pieces) - 1
  let xxs = [BlockRequest (PieceIndex lastPieceIndex) (Begin b) (RequestLength $ min blockSize (remainingLength - b))
            | b <- takeWhile (<remainingLength) $ iterate (+blockSize) 0]
  ys ++ xxs

getPieceList :: Tracker.Tracker -> [WorkMessage]
getPieceList tracker = Work <$> (L.groupBy (\(BlockRequest (PieceIndex x) _ _ ) (BlockRequest (PieceIndex y) _ _ ) -> x == y) $ getRequestList tracker)

responseToMaybeRequest :: ResponseMessage -> Maybe WorkMessage
responseToMaybeRequest (Failed xs) = Just (Work xs)
responseToMaybeRequest _ = Nothing

loop tracker trackerResponse workChan responseChan peers pieceResponseCounter killChan = do
  print $ "RESPONSE CHANNEL: number of active peers: " ++ (show $ length peers)
  response <- Chan.readChan responseChan
  print $ "RESPONSE CHANNEL: " ++ (show response)
  let Tracker.SingleFileInfo (Tracker.Name fileName) (Tracker.Length fileLength) _ = getTrackerSingleFileInfo tracker

  (newPeers, newPieceResponseCounter) <- case response of
    (Succeeded (PieceResponse (PieceIndex i) (PieceContent c))) -> do
      let hash = shaHash c
      BS.writeFile (UTF8.toString hash) c
      print $ "WROTE " ++ (UTF8.toString hash)
      writePiece tracker (UTF8.toString fileName) (PieceResponse (PieceIndex i) (PieceContent c))
      return (peers, pieceResponseCounter + 1)
    (Error p) -> do
      let n = filter (/=p) peers
      print $ "RESPONSE CHANNEL: Peer " ++ show p ++ " is no longer running. new peer list: " ++ show n
      return (n, pieceResponseCounter)
    _ ->
      return (peers, pieceResponseCounter)

  case responseToMaybeRequest response of
    (Just work) ->
      Chan.writeChan workChan work
    _ ->
      return ()

  print $ "Downloaded " ++ (show ((fromIntegral newPieceResponseCounter) / (fromIntegral $ length $ getPieceList tracker))) ++ "%"
  when (allPiecesWritten newPieceResponseCounter) $ do
    print "DONE!"
    Chan.writeChan killChan ()

  loop tracker trackerResponse workChan responseChan newPeers newPieceResponseCounter killChan
  where allPiecesWritten = (==) 1--(fromIntegral (length $ getPieceList tracker))


start :: Tracker.Tracker -> Tracker.TrackerResponse -> Chan.Chan WorkMessage -> Chan.Chan ResponseMessage -> [Peer] -> Chan.Chan () -> IO ()
start tracker trackerResponse workChan responseChan peers killChan = do

  --putStrLn $ "ADDING WORK TO CHANNEL: " ++ (show pieceList)
  createFile $ getTrackerSingleFileInfo tracker
  Chan.writeList2Chan workChan (getPieceList tracker)
  loop tracker trackerResponse workChan responseChan peers 0 killChan

createFile :: Tracker.SingleFileInfo -> IO ()
createFile (Tracker.SingleFileInfo (Tracker.Name fileName) (Tracker.Length fileLength) _) = SIO.withBinaryFile (UTF8.toString fileName) SIO.WriteMode $ flip SIO.hSetFileSize fileLength

writePiece :: Tracker.Tracker -> SIO.FilePath -> PieceResponse -> IO ()
writePiece tracker filePath (PieceResponse (PieceIndex pieceIndex) (PieceContent content)) = SIO.withBinaryFile filePath SIO.ReadWriteMode f
  where f h = do
          SIO.hSeek h SIO.AbsoluteSeek $ getTrackerPieceLength tracker * pieceIndex
          BS.hPut h content

-- TODO: Make sure you check whether the block request is valid or not before performing it
readBlock :: Tracker.Tracker -> SIO.FilePath -> BlockRequest -> IO (BS.ByteString)
readBlock tracker filePath (BlockRequest (PieceIndex pieceIndex) (Begin begin) (RequestLength rl)) =
  SIO.withBinaryFile filePath SIO.ReadMode f
  where f h = do
          SIO.hSeek h SIO.AbsoluteSeek ((getTrackerPieceLength tracker * pieceIndex) + begin)
          BS.hGet h $ fromIntegral rl


-- TODO: Move this into an hspec
test = do
  Just t <- Tracker.testTracker

  let pieceList = getPieceList t

  let Tracker.SingleFileInfo (Tracker.Name fileName) (Tracker.Length fileLength) _ = getTrackerSingleFileInfo t
  let singleFileInfo = Tracker.getTrackerSingleFileInfo t
  let pieceLength = Tracker.getTrackerPieceLength t

  print $ "singleFileInfo " ++ (show singleFileInfo)
  print $ "pieceLength " ++ (show pieceLength)
  createFile singleFileInfo
  pieceFiles <- Dir.listDirectory "pieces"

  -- Write to the file
  forM_ pieceFiles $ \file -> do
    print file
    let unhexFileName = unhex $ UTF8.fromString file
        pieces = Tracker.getTrackerPieces t
        maybePieceIndex = unhexFileName `L.elemIndex` pieces
    if isJust maybePieceIndex then do
      let pieceIndex = fromIntegral $ fromJust maybePieceIndex
      print "is an elem of pieces"
      print $ "wrinting to index " ++ (show pieceIndex) ++ " offset " ++ (show $ pieceLength * pieceIndex)
      content <- BS.readFile $ "pieces/" <> file
      writePiece t (UTF8.toString fileName) (PieceResponse (PieceIndex pieceIndex) (PieceContent content))
    else
      print "is NOT an elem of pieces"

  -- Read blocks from the file, verify that they sum up to the correct content
  forM_ pieceFiles $ \file -> do
    print file
    let unhexFileName = unhex $ UTF8.fromString file
        pieces = Tracker.getTrackerPieces t
        maybePieceIndex = unhexFileName `L.elemIndex` pieces
    if isJust maybePieceIndex then do
      print "is an elem of pieces"
      let pieceIndex = fromIntegral $ fromJust maybePieceIndex
      let (Work blocks) = pieceList !! (fromIntegral $ pieceIndex)
      print $ "reading index  " ++ (show pieceIndex) ++ " offset " ++ (show $ pieceLength * pieceIndex)
      content <- BS.readFile $ "pieces/" <> file
      byteStrings <- mapM (readBlock t (UTF8.toString fileName)) blocks
      print $ "content matches up: " <> (show ((BS.concat byteStrings) == content))
    else
      print "is NOT an elem of pieces"



