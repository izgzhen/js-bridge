import Network.Simple.TCP
import JS.Type
import Network.Socket (socketToHandle)
import JS.Platform
import System.IO

main = startSession $ \sock -> do
    handler <- socketToHandle sock ReadWriteMode
    invoke handler (LInterface (Name "I")) (Name "f") []
    end handler

