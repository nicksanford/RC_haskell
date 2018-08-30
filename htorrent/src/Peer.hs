{-# LANGUAGE OverloadedStrings #-}
module Peer where

import qualified Tracker                   as T (InfoHash (..), Peer (..),
                                                 PeerId (..), Tracker (..),
                                                 getTrackerPieces)
import qualified FileManager               as FM
import           Utils                     (unhex)

import           Network.Socket            hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)
import qualified Control.Concurrent.Chan as Chan

import qualified Data.Bits                 as Bits
import qualified Data.Word8                as W

import qualified Data.ByteString           as BS
import qualified Data.ByteString.UTF8      as UTF8
import qualified Data.ByteString.Base16    as B16
import qualified Data.Either               as Either

import           Control.Monad             (unless, forM_, when)
import           Data.List                 (find, foldl')
import qualified Data.Map                  as M

import           Data.Maybe                (fromJust, isNothing, listToMaybe, fromMaybe, isJust)
import qualified Data.Set as S


newtype Conn e = Conn e deriving (Eq, Show)
newtype InfoHash e = InfoHash e deriving (Eq, Show)
newtype PeerId e = PeerId e deriving (Eq, Show)

showPeerId :: BS.ByteString -> String
showPeerId = UTF8.toString . B16.encode 

data PeerResponse = PeerResponse (InfoHash BS.ByteString) (PeerId BS.ByteString) (Conn Socket) deriving (Eq, Show)

-- TODO: I found from trying to download arch that I was getting dupes of piece shas (not sure if I am doing something wrong, but this is to ensure I don't drop pieces b/c the shas are the same)
newtype PieceMap = PieceMap [(BS.ByteString, Bool)] deriving Eq
newtype Chans a = Chans a deriving (Eq)
newtype RPCParse a = RPCParse a deriving (Eq, Show)
newtype OutStandingWork a = OutStandingWork a deriving (Eq, Show)
newtype PeerChoked a = PeerChoked a deriving (Eq, Show)
newtype PeerChoking a = PeerChoking a deriving (Eq, Show)


instance Show PieceMap where
  show (PieceMap m) = "PieceMap len: " ++ (show $ length m) ++ "number of set bits: " ++ (show $ length $ filter snd m)

instance Show (Chans m) where
  show (Chans m) = "Chans" 


data PeerState = PeerState (PeerId BS.ByteString)
                               (Conn Socket)
                               (PieceMap)
                               (Chans (Chan.Chan FM.WorkMessage, Chan.Chan FM.ResponseMessage))
                               (RPCParse PeerRPCParse)
                               (Maybe FM.WorkMessage)
                               (PeerChoked Bool)
                               (PeerChoking Bool)
                   deriving (Eq, Show)

data PeerRPC = PeerKeepAlive
             | Choke
             | UnChoke
             | Interested
             | NotInterested
             | Have BS.ByteString
             | BitField PieceMap
             | Request Integer Integer Integer
             deriving (Eq, Show)

data PeerRPCParse = PeerRPCParse [W.Word8] (Maybe BS.ByteString) [PeerRPC] deriving (Eq, Show)


updatePeerState :: PeerState -> PeerRPCParse -> PeerState 
updatePeerState (PeerState peer_id conn (PieceMap oldPieceMap) chans _ outstanding peerChoked (PeerChoking pc)) (PeerRPCParse w8 e peerRPCs) = do
  let bitfieldUpdatedPieceMap = maybe oldPieceMap (\new -> mergePieceMaps oldPieceMap new)
                                $ (\(BitField (PieceMap piecemap)) -> piecemap)
                                <$> find findBitField peerRPCs
  let pieceMap = foldr updatePieceWithHave bitfieldUpdatedPieceMap peerRPCs
  let newPeerChoking = foldl' updatePeerChoking pc peerRPCs
  let newPeerRPCs = filter clearConsumedRPCs peerRPCs
  (PeerState peer_id conn (PieceMap pieceMap) chans (RPCParse $ PeerRPCParse w8 e newPeerRPCs) outstanding peerChoked (PeerChoking newPeerChoking))
  where
        clearConsumedRPCs (Have _)     = False
        clearConsumedRPCs (BitField _) = False
        clearConsumedRPCs Choke        = False
        clearConsumedRPCs UnChoke      = False
        clearConsumedRPCs _            = True
        updatePieceWithHave (Have key) acc = fmap (\(k,v) -> if k == key then (k,True) else (k,v)) acc
        updatePieceWithHave _ acc = acc
        updatePeerChoking :: Bool -> PeerRPC -> Bool
        updatePeerChoking _ Choke  = True
        updatePeerChoking _ UnChoke = False
        updatePeerChoking acc _ = acc

mergePieceMaps :: [(a, Bool)] -> [(a, Bool)] -> [(a, Bool)]
mergePieceMaps = zipWith (\(x,xbool) (_,ybool) -> (x, xbool || ybool))

workIsEmpty :: PeerState -> Bool
workIsEmpty (PeerState _ _ _ _ _ Nothing _ _) = True
workIsEmpty _                                 = False

addWork :: PeerState -> FM.WorkMessage -> PeerState
addWork (PeerState a b c d e _ f g) work =
         (PeerState a b c d e (Just work) f g)

sendWorkBackToManager :: PeerState -> IO (PeerState)
sendWorkBackToManager (PeerState a b c (Chans (workChan, respChan)) d (Just (FM.Work work)) e f ) = do
  Chan.writeChan respChan (FM.Failed work)
  return $ PeerState a b c (Chans (workChan, respChan)) d Nothing e f
sendWorkBackToManager x = return x
  
peerChoking :: PeerState -> Bool
peerChoking (PeerState _ _ _ _ _ _ _  (PeerChoking peerChoking)) = peerChoking

workChan :: PeerState -> Chan.Chan FM.WorkMessage
workChan (PeerState _ _ _ (Chans (workChan, _)) _ _ _ _) = workChan

resposneChan :: PeerState -> Chan.Chan FM.ResponseMessage
resposneChan (PeerState _ _ _ (Chans (_, respChan)) _ _ _ _) = respChan

peerHasData :: PieceMap -> FM.WorkMessage -> Bool
peerHasData _                   (FM.Work [])                                         = False
peerHasData pieceMap (FM.Work (FM.BlockRequest (FM.PieceIndex p) _ _ :_)) =
  p `elem` (piecesThatCanBeDone pieceMap)

piecesThatCanBeDone :: PieceMap -> [Integer]
piecesThatCanBeDone (PieceMap pieceMap) = fmap fst $ filter (\(_, (k,v)) -> v) $ zip ([0..]) pieceMap

pullUntilHaveValidWork :: PeerState -> IO PeerState
pullUntilHaveValidWork peerState = do
  newPeerState@(PeerState _ _ pieceMap@(PieceMap pm) _ _ (Just workMessage@(FM.Work work)) _ _) <- if workIsEmpty peerState then do
                        print "WORK IS EMPTY PULLING NEW WORK"
                        work <- Chan.readChan $ workChan peerState
                        return $ addWork peerState work
                    else
                      return peerState

  if peerHasData pieceMap workMessage then
    return newPeerState
   else do
     print $ "WORK CAN'T BE DONE, ON INDEX " ++ (show $ listToMaybe work) ++ "BUT CAN BE DONE ON " ++ (show $ piecesThatCanBeDone pieceMap) ++ " SENDING BACK TO MANAGER AND TRYING AGAIN"
     clearedPeerState <- sendWorkBackToManager newPeerState
     pullUntilHaveValidWork clearedPeerState

cantDoWork peerState@(PeerState _ _ pieceMap _  _ _ _ _) =
  (peerChoking peerState) || (piecesThatCanBeDone pieceMap) == []

-- TODO Got to figure out how to send a keep alive to every peer every 30 seconds w/o blocking the thread
recvLoop :: T.Tracker -> PeerState -> IO ()
--recvLoop tracker peerState@(PeerState (PeerId peer_id) (Conn conn) pieceMap) peerRPCParse workChan responseChan currentWork = do
recvLoop tracker peerState@(PeerState (PeerId peer_id) (Conn conn) _ _ (RPCParse peerRPCParse) _ _ _)  = do
  -- TODO If anything throws, catch it, put the work back in the response queue for the parent thread
  print $ "Blocked on recvLoop on peer_id:" ++ showPeerId peer_id
  -- TODO If this is null but you still have checked out work, put the work back, I'm pretty sure I only need to do this on recv calls
  msg <- recv conn 4096

  let newPeerRPCParse@(PeerRPCParse _ maybeErrors _) = parseRPC tracker msg peerRPCParse
  if isJust maybeErrors then do
    -- TODO: clear your work state and send it back to the parent
    putStrLn $ "ERROR: RECVLOOP hit parse error " ++ show newPeerRPCParse
    _ <- sendWorkBackToManager peerState
    return ()
  else
    if BS.null msg then do
      -- TODO: clear your work state and send it back to the parent
      putStrLn "DONE: RECVLOOP got null in receive "
      _ <- sendWorkBackToManager peerState
      return ()
    else do
      let newRPCPeerState = updatePeerState peerState newPeerRPCParse
      putStrLn $ "RECVLOOP after update: " ++ show newRPCPeerState
      if cantDoWork newRPCPeerState then do
        -- TODO: clear your work state and send it back to the parent
        print "CANT DO WORK"
        newRPCPeerStateWithoutWork  <- sendWorkBackToManager peerState
        recvLoop tracker newRPCPeerStateWithoutWork-- newPeerRPCParse workChan responseChan currentWork
      else do
        print "CAN DO WORK, PULLING WORK"
        newPeerState <- pullUntilHaveValidWork newRPCPeerState
        print "GOT WORK"
        -- TODO: request data
        -- TODO: check if I have all the data I need
        -- TODO: send work back to parent thread if I am good


        recvLoop tracker newPeerState-- newPeerRPCParse workChan responseChan currentWork

      -- TODO pull a piece from the channel, if your peer has that piece, workon it, otherwise put it back in the response channel
      -- While working, if the peer has a given piece, request that piece from them, starting by the offset, upon getting a response, continue to request blocks until you have the entire piece, then put the fullpiece on the response channel and pull a new bit of work.



--[0,0,1,3,6,index,begin,length]
-- request: <len=0013><id=6><index><begin><length>

-- The request message is fixed length, and is used to request a block. The payload contains the following information:

--     index: integer specifying the zero-based piece index
--     begin: integer specifying the zero-based byte offset within the piece
--     length: integer specifying the requested length.

findBitField :: PeerRPC -> Bool
findBitField (BitField _) = True
findBitField _            = False

start :: T.Tracker -> T.Peer -> Chan.Chan FM.WorkMessage -> Chan.Chan FM.ResponseMessage -> IO ()
start tracker peer workChan responseChan =  do
  maybePeerResponse <- initiateHandshake tracker peer
  print $ "STARTPEER maybePeerResponse: " ++ show maybePeerResponse
  unless (isNothing maybePeerResponse) $ do
    let peerResponse@(PeerResponse _ (PeerId peer_id) (Conn conn)) = fromJust maybePeerResponse
    print $ "STARTPEER sending interested for: " ++ show peer
    _ <- sendInterested peerResponse

    let peerState = PeerState (PeerId peer_id) (Conn conn) (initPieceMap tracker) (Chans (workChan, responseChan)) (RPCParse defaultPeerRPCParse) Nothing (PeerChoked True) (PeerChoking True)
    recvLoop tracker peerState

defaultPeerRPCParse :: PeerRPCParse
defaultPeerRPCParse = PeerRPCParse [] Nothing []

initPieceMap :: T.Tracker -> PieceMap
initPieceMap t = PieceMap $ (\x -> (x, False)) <$> T.getTrackerPieces t

-- I can start parsing the bitfield so I can start trying to leach data

-- TODO This is going to need to be a
-- parseRPC :: BS.ByteString -> Run BS.ByteString  (Maybe [PeerRPC])
-- B/c you might not yet have all the data you need and multiple messages can be received in a given receive block
-- there are also situations where you might want to drop the connection if the parsing rules are violated
-- Right now this makes the assumption that we are only ever getting a single RPC in a given recv call (which is not true)

initialRPCParse :: PeerRPCParse
initialRPCParse = (PeerRPCParse [] Nothing [])

parseRPC :: T.Tracker -> BS.ByteString -> PeerRPCParse -> PeerRPCParse
parseRPC tracker bs peerRPCParse = do
  foldl' (parseRPC' tracker) peerRPCParse $ BS.unpack bs

parseRPC' tracker acc@(PeerRPCParse word8Buffer Nothing xs) word8
  | newBuffer == [0,0,0,0] = PeerRPCParse [] Nothing (xs ++ [PeerKeepAlive])
  | newBuffer == [0,0,0,1,0] = PeerRPCParse [] Nothing (xs ++ [Choke])
  | newBuffer == [0,0,0,1,1] = PeerRPCParse [] Nothing (xs ++ [UnChoke])
  | newBuffer == [0,0,0,1,2] = PeerRPCParse [] Nothing (xs ++ [Interested])
  | newBuffer == [0,0,0,1,3] = PeerRPCParse [] Nothing (xs ++ [NotInterested])
  | take 5 newBuffer == [0,0,0,5,4] =
    if length newBuffer == 9 then
      PeerRPCParse (drop 9 newBuffer) Nothing (xs ++ [Have $ BS.pack $ take 4 $ drop 5 newBuffer])  --NOTE: I think that this maybe should be (Have $ BS.pack $ take 1 $ drop 5 unpackedMsg)
    else
      PeerRPCParse newBuffer Nothing xs  --NOTE: I think that this maybe should be (Have $ BS.pack $ take 1 $ drop 5 unpackedMsg)
  | take 3 newBuffer == [0,0,0] &&
    (drop 4 (take 5 newBuffer) == [5])  = do--if  == fromIntegral (length $ drop 5 unpackedMsg) then do
                                            let bitfieldLength =  fromIntegral ((newBuffer !! 3) - 1)
                                            let word8s = take bitfieldLength (drop 5 newBuffer)
                                            let boolsBeforeCheck = word8s >>= (\ x -> reverse [Bits.testBit x i | i <- [0 .. 7]])

                                            if (ceiling ((fromIntegral . length $ T.getTrackerPieces tracker) / 8)) /= bitfieldLength then
                                              PeerRPCParse newBuffer (Just "ERROR parseRPC in BitField parse, (ceiling ((fromIntegral . length $ T.getTrackerPieces tracker) / 8)) /= bitfieldLength") xs
                                            else do
                                              --if (S.size (S.fromList (T.getTrackerPieces tracker)) /= (length (T.getTrackerPieces tracker))) then
                                               -- PeerRPCParse newBuffer (Just "ERROR parseRPC in BitField parse, (S.size (S.fromList (T.getTrackerPieces tracker)) /= (length (T.getTrackerPieces tracker)))") xs
                                              --else do
                                              -- If the number of bools are lower than the number of pieces then we don't have enough data to proceed
                                              if length word8s /= bitfieldLength then do
                                                PeerRPCParse newBuffer Nothing xs
                                              else do
                                                let extraBits = drop (length $ T.getTrackerPieces tracker) boolsBeforeCheck

                                                -- If we have any extra bits we should  drop the connection
                                                if or extraBits then
                                                  PeerRPCParse newBuffer (Just "ERROR parseRPC in BitField parse, extra bits are set") xs
                                                else
                                                  PeerRPCParse [] Nothing (xs ++ [BitField $ PieceMap $ zip (T.getTrackerPieces tracker) boolsBeforeCheck])
  | take 5 newBuffer == [0,0,1,3,6] = if length newBuffer == 8 then
                                          PeerRPCParse [] Nothing (xs ++ [Request (fromIntegral $ newBuffer !! 5) (fromIntegral $ newBuffer !! 6) (fromIntegral $ newBuffer !! 7)])
                                        else
                                          PeerRPCParse newBuffer Nothing xs
  | otherwise = PeerRPCParse newBuffer Nothing xs
  where newBuffer = word8Buffer ++ [word8]
parseRPC' tracker (PeerRPCParse word8Buffer error xs) word8 = PeerRPCParse newBuffer error xs
  where newBuffer = word8Buffer ++ [word8]


keepAlive :: BS.ByteString
keepAlive = BS.pack [0,0,0,0]

choke :: BS.ByteString
choke = BS.pack [0,0,0,1,0]

unchoke :: BS.ByteString
unchoke = BS.pack [0,0,0,1,1]

interested :: BS.ByteString
interested = BS.pack [0,0,0,1,2]

notInterested :: BS.ByteString
notInterested = BS.pack [0,0,0,1,3]

have :: BS.ByteString -> BS.ByteString
have pieceIndex = BS.concat [BS.pack [0,0,0,5,4], pieceIndex]

--bitfield :: BS.ByteString -> BS.ByteString
--bitfield payload = BS.concat [BS.pack [0,0,0,1 + (fromIntegral . BS.length $ payload), 5], payload]

isValidBitField :: BS.ByteString -> Bool
isValidBitField bs = len == fromIntegral (length $ drop 5 xs)
  where xs = BS.unpack bs
        len = xs !! 3 - 1

request :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
request index begin len = BS.concat [BS.pack [0,0,1,3,6], index, begin, len]

sendInterested :: PeerResponse -> IO ()
sendInterested pr@(PeerResponse (InfoHash peer_info_hash) (PeerId peer_peer_id) (Conn conn)) = do
  sendAll conn interested

trackerToPeerHandshake :: T.Tracker -> BS.ByteString
trackerToPeerHandshake (T.Tracker (T.PeerId peer_id) _ _ _ (T.InfoHash info_hash) _ _ _) =
  handshake info_hash peer_id

initiateHandshake :: T.Tracker -> T.Peer -> IO (Maybe PeerResponse)
initiateHandshake tracker peer = do
  (response, conn) <- sendHandshake peer $ trackerToPeerHandshake tracker
  return $ readHandShake conn response >>= validateHandshake tracker

handshake :: BS.ByteString -> BS.ByteString -> BS.ByteString
handshake info_hash peer_id =  BS.concat [pstrlen, pstr, reserved, unhex info_hash, peer_id]
  where pstr = "BitTorrent protocol"
        pstrlen = BS.singleton $ fromIntegral (BS.length pstr)
        reserved = BS.replicate 8 W._nul


-- TODO: If the peer_id differs from what the tracker sent (if the tracker sent
-- anything regarding the peer_id) then it should be dropped.
-- TODO try to send 16k blocks
-- Try downloading pieces in random order first, then, as an optimization, try rairest first.
-- Only change choaked peers once every 10 seconds (to prevent fibralation)
readHandShake :: Socket -> BS.ByteString  ->  Maybe PeerResponse
readHandShake conn r = PeerResponse <$> (InfoHash <$> infoHash) <*> (PeerId <$> peerId) <*> (Conn <$> Just conn)
  where word8s = BS.unpack r
        afterProtocol :: Maybe BS.ByteString
        afterProtocol = BS.pack . flip drop word8s . (1+) . fromIntegral <$> listToMaybe word8s
        afterEmpty :: Maybe BS.ByteString
        afterEmpty = BS.drop 8 <$> afterProtocol
        infoHash :: Maybe BS.ByteString
        infoHash = BS.take 20 <$> afterEmpty
        peerId :: Maybe BS.ByteString
        peerId = BS.drop 20 <$> afterEmpty

validateHandshake :: T.Tracker -> PeerResponse -> Maybe PeerResponse
validateHandshake (T.Tracker _ _ _ _ (T.InfoHash info_hash) _ _ _)
                  pr@(PeerResponse (InfoHash peer_info_hash) (PeerId peer_peer_id) _) =
                  if unhex info_hash == peer_info_hash then
                    Just pr
                  else
                    Nothing


-- This can return whatever you want (I believe including the socket)
sendHandshake :: T.Peer -> BS.ByteString -> IO (BS.ByteString, Socket)
sendHandshake (T.Peer ip port) bs = do
  --print "sending"
  sock <- addrIO >>= open
  sendAll sock bs
  --print $ BS.concat ["sent: ", bs]
  -- This is to make it so that I don't overfetch here, given that I know
  -- exactly how many bytes I need to read in for the handshake response.
  -- 49 + (length "BitTorrent protocol") == 58
  msg <- recv sock 68
  return (msg, sock)

  where hints = defaultHints { addrSocketType = Stream }
        addrIO = head <$> getAddrInfo (Just hints) (Just $ UTF8.toString ip) (Just $ show port)
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          -- print "in open addr"
          -- print "addr"
          -- print addr
          -- print "sock"
          -- print sock
          return sock
