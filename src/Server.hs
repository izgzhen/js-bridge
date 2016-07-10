import Network.Simple.TCP
import Control.Concurrent


main = serve (Host "localhost") "8888" $ \(sock, addr) -> do
    putStrLn $ "TCP connection established from " ++ show addr
    handle sock

handle sock = loop
    where
        loop = do
            threadDelay 100000
            msg <- recv sock 1000
            putStrLn $ show msg
            loop

