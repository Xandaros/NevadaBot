module NevadaBot ( NevadaBot(..)
                 , runNevadaBot
                 , rawSend
                 , rawRead
                 , readCommand
                 , joinChannel
                 , getChannels
                 , getNickname
                 , getPlugins
                 , quit
                 ) where
import Parser
import Types

import Control.Concurrent.MVar (MVar(..))
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader(runReaderT)
import System.IO

runNevadaBot :: NevadaBot a -> IRCServer -> IO a
runNevadaBot nevadaBot = runReaderT $ unNevadaBot nevadaBot

rawSend :: String -> NevadaBot ()
rawSend s = do
    sock <- asks serverHandle
    liftIO $ hPutStr sock (s ++ "\r\n")
    liftIO . putStrLn $ "rawSend: " ++ s
    return ()

rawRead :: NevadaBot String
rawRead = asks serverHandle >>= liftIO . hGetLine

readCommand :: NevadaBot (Maybe IRCMessage)
readCommand = do
    msg <- rawRead 
    let res = parse messageParser "" msg
    case res of
        Left a  -> liftIO $ do
            putStrLn ("Error parsing: " ++ show a)
            putStrLn $ "Nessage: " ++ msg
            return Nothing
        Right a -> return $ Just a

joinChannel :: String -> NevadaBot ()
joinChannel = rawSend . ("JOIN "++)

getChannels :: NevadaBot [Channel]
getChannels = asks channels

getNickname :: NevadaBot User
getNickname = asks nickname

getPlugins :: NevadaBot [Plugin]
getPlugins = asks plugins

quit :: String -> NevadaBot ()
quit s = rawSend ("QUIT :" ++ s) >> asks serverHandle >>= liftIO . hClose
