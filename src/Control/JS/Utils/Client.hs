module Control.JS.Utils.Client where

import Language.JS.Type
import Language.JS.Platform

import Network.Simple.TCP
import Network.Socket (socketToHandle)
import System.IO

startSession :: Domains -> String -> (PlatPort -> IO a) -> IO a
startSession domains idl f = connect "localhost" "8888" $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    handler <- socketToHandle sock ReadWriteMode
    putStrLn "[SEND DOMAINS]"
    ePutLine handler (CBoot domains idl)
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
call handler lvar x args = sendCmd handler $ CCall lvar x args

get :: Handle -> LVar -> Name -> IO Reply
get handler lvar x = sendCmd handler $ CGet lvar x

set :: Handle -> LVar -> Name -> JsVal -> IO Reply
set handler lvar x val = sendCmd handler $ CSet lvar x val

newDef :: Handle -> Name -> IO Reply
newDef handler x = sendCmd handler (CNew x Nothing)

newCons :: Handle -> Name -> [JsVal] -> IO Reply
newCons handler iface args = sendCmd handler (CNew iface (Just args))

end :: Handle -> IO ()
end handler = do
  let cmd = CEnd
  putStrLn $ "[SEND REQ] " ++ show cmd
  ePutLine handler cmd
