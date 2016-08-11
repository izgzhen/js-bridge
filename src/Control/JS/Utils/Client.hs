module Control.JS.Utils.Client where

import Language.JS.Type
import Language.JS.Platform

import Network.Simple.TCP
import Network.Socket (socketToHandle)
import System.IO

startSession :: Domains -> (PlatPort -> IO a) -> IO a
startSession domains f = connect "localhost" "8888" $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    handler <- socketToHandle sock ReadWriteMode
    putStrLn "[SEND DOMAINS]"
    ePutLine handler (CBoot domains)
    ret <- f handler
    end handler
    return ret

sendCmd :: Handle -> Command -> IO Reply
sendCmd handler cmd = do
  putStrLn $ "[SEND REQ] " ++ show cmd
  ePutLine handler cmd
  mReply <- eGetLine handler
  case mReply of
      Just reply -> do
        putStrLn $ "[GET RES] " ++ show reply
        return reply
      Nothing -> return InvalidReqeust

call :: Handle -> LVar -> Name -> [JsVal] -> IO Reply
call handler lvar name es = sendCmd handler (CCall lvar name es)

end :: Handle -> IO ()
end handler = do
  let cmd = CEnd
  putStrLn $ "[SEND REQ] " ++ show cmd
  ePutLine handler cmd
