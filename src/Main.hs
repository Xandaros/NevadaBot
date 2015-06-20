{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import           Control.Concurrent (ThreadId, forkIO, throwTo, threadDelay)
import           Control.Exception (AsyncException(UserInterrupt))
import           Control.Monad
import           Control.Monad.Catch hiding (Handler)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Network
import           Safe
import           System.IO

import           Types
import           NevadaBot

activeChannels :: [Channel]
activeChannels = [ "#nevadaTest"
                 ]

testPlugin :: Plugin
testPlugin = Plugin handlers meta
    where
        handlers = [testHandler, joinHandler, pingHandler]
        meta = PluginMeta "Test Plugin" "" "Xandaros" (Version 0 1 0)

greeter :: Plugin
greeter = Plugin [handler] meta
    where
        meta = PluginMeta "Greeter" "" "Xandaros" (Version 0 1 0)
        handler = TempHandler $ \(IRCMessage prefix (UNKNOWNMSG cmd args)) ->
            when (cmd == "JOIN") $ do
                let nick = T.unpack . fromMaybe "" $ preNickname <$> prefix
                let channel = T.unpack $ atDef "" args 0
                rawSend $ "NOTICE " ++ channel ++ " :Hello, " ++ nick ++ "!"

ircServer :: IRCServer
ircServer = IRCServer undefined activeChannels "NevadaTest" plugins
    where
        plugins = [testPlugin, greeter]

testHandler :: Handler
testHandler = TempHandler $ \command ->
    liftIO $ print command

joinHandler :: Handler
joinHandler = TempHandler $ \IRCMessage{..} -> do
    let UNKNOWNMSG cmd args = command
    when (cmd == "376") $
            joinChannel "#XanTest"

pingHandler :: Handler
pingHandler = TempHandler $ \IRCMessage{..} -> do
    let UNKNOWNMSG cmd args = command
    when (cmd == "PING") $
        rawSend $ "PONG" ++ (T.unpack . (" "<>) $ atDef "" args 0)

runHandlers :: IRCMessage -> NevadaBot ()
runHandlers cmd = do
    pls <- getPlugins
    sequence_ $ do
        pl <- pls -- for each pl in pls (plugins)
        let handlers' = handlers pl
        do
            handler <- handlers' -- for each handler in handlers'
            case handler of
                TempHandler f -> return $ f cmd
                _ -> return $ return ()
    return ()

mainBot :: NevadaBot ()
mainBot = do
    rawSend "NICK XanTest"
    rawSend "USER XanTest 0 * :Xan Test"
    let loop = forever $ do
            Just line <- readCommand
            runHandlers line
    catch loop $ \UserInterrupt -> quit "Killed by console"

server = "chat.freenode.net"
port = PortNumber 6667

main :: IO ()
main = withSocketsDo $ do
    handle <- connectTo server port
    hSetBuffering handle LineBuffering 
    let ircServ = ircServer{serverHandle = handle}
    runNevadaBot mainBot ircServ
