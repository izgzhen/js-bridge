import System.IO
import Control.Concurrent (threadDelay)
import Text.Read (readMaybe)
import Language.JS.Platform

main = withFile "/Users/zhenzhang/testpipe" ReadWriteMode loop
    where
        loop h = do
            eof <- hIsEOF h
            if eof then do
                putStrLn "EOF ... waiting ..."
                threadDelay 1000000
                loop h
                else do
                    l <- hGetLine h
                    putStrLn l
                    if l == "***START***"
                        then do
                            putStrLn "Start protocol"
                            Just (Domains doms) <- eGetLine h
                            print doms
                            serve h
                        else loop h

serve h = do
    c <- (eGetLine h :: IO (Maybe Command))
    case c of
        Just cmd -> print cmd
        Nothing -> putStrLn "Invalid line"
    serve h
