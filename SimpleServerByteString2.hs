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
import Network.Socket.ByteString (recv, sendAll)
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
worker sock = loop expectedRequestLength
  where
    loop :: Int -> IO ()
    loop !left
      | left == 0 = do sendAll sock reply
                       loop expectedRequestLength
      | otherwise = do bs' <- recv sock left
                       if B.length bs' == 0
                         then return ()
                         else loop $ left - B.length bs'

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
 
