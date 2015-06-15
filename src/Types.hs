{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Types ( IRCServer(..)
             , NevadaBot(..) -- re-export from NevadaBot
             , Channel(..)
             , Version(..)
             , PluginMeta(..)
             , Plugin(..)
             , User(..)
             , Recipient(..)
             , Handler(..)
             , IRCMessage(..)
             , IRCPrefix(..)
             , IRCCommand(..)
             ) where

import           Control.Monad.Catch(MonadThrow, MonadCatch)
import           Control.Monad.Reader
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import qualified Data.Text as T
import           Network
import           System.IO

newtype NevadaBot a = NevadaBot { unNevadaBot :: ReaderT IRCServer IO a }
    deriving ( Functor   , Applicative, Monad, MonadIO, MonadReader IRCServer
             , MonadThrow, MonadCatch
             )

data IRCServer = IRCServer { serverHandle :: Handle
                           , channels     :: [Channel]
                           , nickname     :: User
                           , plugins      :: [Plugin]
                           }

type Channel = T.Text

data Version = Version Int Int Int

data PluginMeta = PluginMeta { pluginName        :: T.Text
                             , pluginDescription :: T.Text
                             , pluginAuthor      :: T.Text
                             , pluginVersion     :: Version
                             }

data Plugin = Plugin { handlers   :: [Handler]
                     , pluginMeta :: PluginMeta
                     }

type User = T.Text

data Recipient = RUser User
               | RChannel Channel

data Handler = TempHandler (IRCMessage -> NevadaBot ())

--data Handler = PrivMsgHandler (User -> Recipient -> T.Text        -> NevadaBot ())
--             | JoinHandler    (User -> Channel   -> NevadaBot ())
--             | PartHandler    (User -> Channel   -> NevadaBot ())

----------------------

instance Show Version where
    show (Version maj min pat) = show maj ++ "." ++ show min ++ "." ++ show pat

instance Show PluginMeta where
    show PluginMeta{..} = T.unpack pluginName ++ " (v" ++ show pluginVersion ++ " by " ++ T.unpack pluginAuthor ++ ")"

instance Show Plugin where
    show = show . pluginMeta

instance Show IRCServer where
    show IRCServer{..} = T.unpack nickname ++ " " ++ show channels



--------------------------------------------------------------------------------

data IRCPrefix = IRCPrefix { preNickname :: User
                           , preUsername :: Maybe User
                           , preHostname :: Maybe T.Text
                           }
    deriving (Show)

data IRCMessage = IRCMessage { prefix  :: Maybe IRCPrefix
                             , command :: IRCCommand
                             }
    deriving (Show)

data IRCCommand = IRCCommand T.Text [T.Text]
    deriving (Show)

--data IRCCommand = PRIVMSG User T.Text
--                | UNKNOWNMSGT T.Text
--                | UNKNOWNMSGI Int
--    deriving (Show)
