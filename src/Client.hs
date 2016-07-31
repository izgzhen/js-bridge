import Network.Simple.TCP
import JS.Type
import Network.Socket (socketToHandle)
import JS.Platform
import System.IO

main = startSession $ \handler -> do
    _ <- eval handler (JNew (Name "Bar") []) Nothing
    return ()
