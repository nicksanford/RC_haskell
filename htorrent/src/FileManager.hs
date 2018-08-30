{-# LANGUAGE OverloadedStrings #-}
module FileManager where
import Tracker
import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString as BS
import qualified Data.List as L

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
blockSize = 2^14 -- 16k
-- TODO: I am going to start off with only implementing this for a single file, in multifile mode I will need to maintain multiple files on the filesystem, I will deal with that once I have gotten single file downloads & uploads working.

newtype PieceIndex a = PieceIndex a deriving (Eq, Show, Ord)
newtype Begin a = Begin a deriving (Eq, Show, Ord)
newtype RequestLength a = RequestLength a deriving (Eq, Show, Ord)
newtype PieceContent a = PieceContent a deriving (Eq, Show, Ord)

data BlockRequest = BlockRequest (PieceIndex Integer) (Begin Integer) (RequestLength Integer) deriving (Eq, Show, Ord)

data PieceResponse = PieceResponse(PieceIndex Integer) (PieceContent BS.ByteString) deriving (Eq, Show, Ord)

data WorkMessage = Work [BlockRequest] deriving (Eq, Show)

data ResponseMessage = Failed [BlockRequest]
                     | Succeeded PieceResponse
                     deriving (Eq, Show, Ord)

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

responseToMaybeRequest :: ResponseMessage -> Maybe WorkMessage
responseToMaybeRequest (Failed xs) = Just (Work xs)
responseToMaybeRequest _ = Nothing

loop tracker trackerResponse workChan responseChan = do
  response <- Chan.readChan responseChan
  print $ "RESPONSE CHANNEL: " ++ (show response)
  case responseToMaybeRequest response of
    (Just work) ->
      Chan.writeChan workChan work
    _ ->
      return ()
  loop tracker trackerResponse workChan responseChan

start :: Tracker.Tracker -> Tracker.TrackerResponse -> Chan.Chan WorkMessage -> Chan.Chan ResponseMessage -> IO ()
start tracker trackerResponse workChan responseChan = do
  let pieceList :: [WorkMessage]
      pieceList = Work <$> (L.groupBy (\(BlockRequest (PieceIndex x) _ _ ) (BlockRequest (PieceIndex y) _ _ ) -> x == y) $ getRequestList tracker)

  --putStrLn $ "ADDING WORK TO CHANNEL: " ++ (show pieceList)
  Chan.writeList2Chan workChan pieceList
  loop tracker trackerResponse workChan responseChan
