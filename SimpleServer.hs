{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

import Control.Concurrent (forkIO, threadWaitRead, threadWaitWrite)
import Control.Monad (when, forever, liftM)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.Internal (toForeignPtr)
import Foreign
import Foreign.C.Types
import Network.Socket (Socket(..), SocketType(..), AddrInfoFlag(..),
                       SocketOption(..), accept, addrAddress, addrFamily,
                       addrFlags, bindSocket, defaultProtocol, defaultHints,
                       fdSocket, getAddrInfo, listen, setSocketOption, socket)
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)
import System.Environment (getArgs)


main = do
  listenSock <- startListenSock
  forever $ do    
    (sock, _) <- accept listenSock
    forkIO $ worker sock

  
startListenSock :: IO Socket
startListenSock = do
  args <- getArgs
  let portNumber = head args
  addrinfos  <- getAddrInfo
                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                Nothing
                (Just $ portNumber)
  let serveraddr = head addrinfos
  listenSock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket listenSock $ addrAddress serveraddr
  setSocketOption listenSock ReuseAddr 1
  listen listenSock listenQueueLength
  return listenSock
  where
    listenQueueLength :: Int
    listenQueueLength = 8192 
 


worker :: Socket -> IO ()
worker sock = do
  recvBuffer <- mallocForeignPtrBytes recvBufferSize
  let (replyFPtr,_,_) = toForeignPtr reply
  withForeignPtr replyFPtr $
    withForeignPtr recvBuffer . serve sock


serve :: Socket -> Ptr Word8 -> Ptr Word8 -> IO ()
serve sock replyPtr recvPtr = loop expectedRequestLength
  where
    loop :: Int -> IO ()
    loop !left
      | left == 0 = do sendAll sock replyPtr replyLen
                       loop expectedRequestLength
      | otherwise = do n <- socketRecv sock recvPtr left
                       if n == 0
                         then return ()
                         else loop $ left - n

-- REPLY   
reply :: ByteString
reply = B.append fauxHeader fauxIndex
 
replyLen :: Int
replyLen = B.length reply 

fauxHeader :: ByteString
fauxHeader = pack s
  where
    s = "HTTP/1.1 200 OK\r\nDate: Tue, 09 Oct 2012 16:36:18 GMT\r\nContent-Length: 151\r\nServer: Mighttpd/2.8.1\r\nLast-Modified: Mon, 09 Jul 2012 03:42:33 GMT\r\nContent-Type: text/html\r\n\r\n"
 
fauxIndex :: ByteString
fauxIndex = pack s
  where
    s = "<html>\n<head>\n<title>Welcome to nginx!</title>\n</head>\n<body bgcolor=\"white\" text=\"black\">\n<center><h1>Welcome to nginx!</h1></center>\n</body>\n</html>\n"
 
 
-- EXPECTED REQUEST
expectedRequest :: ByteString    
expectedRequest =
  pack "GET / HTTP/1.1\r\nHost: 10.12.0.1:8080\r\nUser-Agent: weighttp/0.3\r\nConnection: keep-alive\r\n\r\n"
 
expectedRequestLength :: Int
expectedRequestLength = B.length expectedRequest
 
recvBufferSize :: Int
recvBufferSize = expectedRequestLength + 100 -- 100 extra bytes for good measure.
 


socketRecv :: Socket         -- ^ Connected socket
              -> Ptr Word8
              -> Int            -- ^ Maximum number of bytes to receive
              -> IO Int
socketRecv socket ptr !nbytes
    | nbytes < 0 = error "socketRecv"
    | otherwise  = recvInner (fdSocket socket) nbytes ptr
        
recvInner :: CInt -> Int -> Ptr Word8 -> IO Int
recvInner s !nbytes ptr =
    fmap fromIntegral $
        throwSocketErrorIfMinus1RetryMayBlock "recvInner"
        (threadWaitRead (fromIntegral s)) $
        c_recv s (castPtr ptr) (fromIntegral nbytes) 0
 
foreign import ccall unsafe "recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
 
foreign import ccall unsafe "send"
  c_send :: CInt -> Ptr a -> CSize -> CInt -> IO CInt
 
send' :: Socket         -- ^ Connected socket
         -> Ptr a       -- ^ Pointer to beginning of data to send
         -> Int         -- ^ Amount of data to send
         -> IO Int      -- ^ Number of bytes sent
send' (MkSocket s _ _ _ _) ptr len =
    liftM fromIntegral $
        throwSocketErrorIfMinus1RetryMayBlock "send'"
        (threadWaitWrite $ fromIntegral s) $
        c_send s ptr (fromIntegral len) 0
 
sendAll :: Socket      -- ^ Connected socket
           -> Ptr a 
           -> Int
           -> IO ()
sendAll !sock !ptr !len = do
    sent <- send' sock ptr len
    when (sent < len) $ sendAll sock (ptr `plusPtr` sent) (len - sent)