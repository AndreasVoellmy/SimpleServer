{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
 
-- ghc --make -O2 -funbox-strict-fields -threaded -rtsopts SimpleServer.hs 
 
import Network.Socket
import System.Environment (getArgs)
import Control.Monad
import Foreign
import Foreign.C.Types
import Network.Socket.Internal (throwSocketErrorIfMinus1RetryMayBlock)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as B
import Control.Concurrent
import Text.Printf (printf)
 
 
listenQueueLength :: Int
listenQueueLength = 8192 
 
main = do
  args <- getArgs
  listenSock <- startListenSock (head args)
  acceptLoop listenSock 0
  where
    acceptLoop :: Socket -> Int -> IO ()
    acceptLoop listenSock !n = do    
        (connsock, clientaddr) <- accept listenSock
        -- printf "%d:%s\n" n (show clientaddr)
        forkIO (serve connsock)
        acceptLoop listenSock (n+1)
  
 
startListenSock :: String -> IO Socket
startListenSock portNumber = do
  addrinfos  <- getAddrInfo
                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                Nothing
                (Just $ portNumber)
  let serveraddr = head addrinfos
  listenSock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket listenSock (addrAddress serveraddr)      
  setSocketOption listenSock ReuseAddr 1
  listen listenSock listenQueueLength
  return listenSock
 
fauxHeader :: B.ByteString
fauxHeader = BC.pack s
  where
    s = "HTTP/1.1 200 OK\r\nDate: Tue, 09 Oct 2012 16:36:18 GMT\r\nContent-Length: 151\r\nServer: Mighttpd/2.8.1\r\nLast-Modified: Mon, 09 Jul 2012 03:42:33 GMT\r\nContent-Type: text/html\r\n\r\n"
 
fauxIndex :: B.ByteString
fauxIndex = BC.pack s
  where
    s = "<html>\n<head>\n<title>Welcome to nginx!</title>\n</head>\n<body bgcolor=\"white\" text=\"black\">\n<center><h1>Welcome to nginx!</h1></center>\n</body>\n</html>\n"
 
fixedReply :: B.ByteString
fixedReply = B.append fauxHeader fauxIndex
 
fixedReplyLength :: Int
fixedReplyLength = B.length fixedReply 
 
-- Expecting the following request
expectedRequest :: BC.ByteString    
expectedRequest =
  BC.pack "GET / HTTP/1.1\r\nHost: 10.12.0.1:8080\r\nUser-Agent: weighttp/0.3\r\nConnection: keep-alive\r\n\r\n"
 
expectedRequestLength :: Int
expectedRequestLength = B.length expectedRequest
 
recvBufferSize :: Int
recvBufferSize = expectedRequestLength + 100 -- 100 extra bytes for good measure.
 
serve :: Socket -> IO ()
serve sock = do
  recvBuffer <- mallocForeignPtrBytes recvBufferSize
  let (fixedReplyFPtr,_,_) = B.toForeignPtr fixedReply
  withForeignPtr fixedReplyFPtr $ \replyPtr ->
    withForeignPtr recvBuffer $ serveAux sock replyPtr 
 
serveAux :: Socket -> Ptr Word8 -> Ptr Word8 -> IO ()
serveAux sock fixedReplyPtr recvPtr = serveRequests
  where
    serveRequests = recv90 expectedRequestLength
    recv90 :: Int -> IO ()
    recv90 !left
      | left == 0 = do reply
                       recv90 expectedRequestLength
      | otherwise = do 
        n <- socketRecv sock recvPtr left
        if n == 0
          then return ()
          else do when (n < 90) $ printf "incomplete receive: %d bytes\n" n
                  recv90 (left - n)
    reply = sendAll' sock fixedReplyPtr fixedReplyLength
 
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
 
send' :: Socket      -- ^ Connected socket
         -> Ptr a       -- ^ Pointer to beginning of data to send
         -> Int       -- ^ Amount of data to send
         -> IO Int      -- ^ Number of bytes sent
send' (MkSocket s _ _ _ _) ptr len =
    liftM fromIntegral $
        throwSocketErrorIfMinus1RetryMayBlock "send'"
        (threadWaitWrite (fromIntegral s)) $
        c_send s ptr (fromIntegral len) 0
 
sendAll' :: Socket      -- ^ Connected socket
            -> Ptr a 
            -> Int
            -> IO ()
sendAll' !sock !ptr !len = do
    sent <- send' sock ptr len
    when (sent < len) $ sendAll' sock (ptr `plusPtr` sent) (len - sent) 