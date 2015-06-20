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
    deriving (Show)

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

--data IRCCommand = IRCCommand T.Text [T.Text]
--    deriving (Show)

data IRCCommand = PRIVMSG Recipient T.Text
                | NICK User
                | MODE User T.Text
                | QUIT (Maybe T.Text)
                | JOIN [Channel]
                | PART [Channel] (Maybe T.Text)
                | TOPIC Channel (Maybe T.Text)
                | NAMES [Channel] (Maybe T.Text)
                | LIST [Channel] (Maybe T.Text)
                | INVITE User Channel
                | KICK [Channel] [User] (Maybe T.Text)
                | NOTICE Recipient T.Text
                | MOTD (Maybe T.Text)
                | LUSERS (Maybe T.Text) (Maybe T.Text)
                | VERSION (Maybe T.Text)
                | STATS (Maybe T.Text) (Maybe T.Text)
                | TIME [T.Text]
                | WHO (Maybe T.Text) Bool
                | WHOIS (Maybe T.Text) [T.Text]
                | WHOWAS [User] (Maybe Int) (Maybe T.Text)
                | AWAY (Maybe T.Text)
                | USERS (Maybe T.Text)
                | WALLOPS T.Text
                | USERHOST [User]
                | ISON [User]
                | PING [T.Text]
                | PONG [T.Text]
                | UNKNOWNMSG T.Text [T.Text]

                -- TODO: Parsers for these...
                | RPL_WELCOME T.Text
                | RPL_YOURHOST T.Text
                | RPL_CREATED T.Text
                | RPL_MYINFO T.Text
                | RPL_BOUNCE T.Text
                | RPL_USERHOST T.Text
                | RPL_ISON T.Text
                | RPL_AWAY T.Text
                | RPL_UNAWAY T.Text
                | RPL_NOWAWAY
                | RPL_WHOISUSER T.Text
                | RPL_WHOISSERVER T.Text
                | RPL_WHOISOPERATOR T.Text
                | RPL_WHOISIDLE T.Text
                | RPL_ENDOFWHOIS
                | RPL_WHOISCHANNELS T.Text
                | RPL_WHOWASUSER T.Text
                | RPL_ENDOFWHOWAS
                | RPL_LIST T.Text
                | RPL_LISTEND
                | RPL_UNIQOPIS T.Text
                | RPL_CHANNELMODEIS T.Text
                | RPL_NOTOPIC
                | RPL_TOPIC T.Text
                | RPL_INVITING T.Text
                | RPL_SUMMONING T.Text
                | RPL_INVITELIST T.Text
                | RPL_ENDOFINVITELIST
                | RPL_EXCEPTLIST T.Text
                | RPL_ENDOFEXCEPTLIST
                | RPL_VERSION T.Text
                | RPL_WHOREPLY T.Text
                | RPL_ENDOFWHO
                | RPL_NAMREPLY T.Text
                | RPL_ENDOFNAMES
                | RPL_LINKS T.Text
                | RPL_ENDOFLINKS
                | RPL_BANLIST T.Text
                | RPL_ENDOFBANLIST
                | RPL_INFO T.Text
                | RPL_ENDOFINFO
                | RPL_MOTDSTART
                | RPL_MOTD T.Text
                | RPL_ENDOFMOTD
                | RPL_YOUREOPER
                | RPL_REHASHING T.Text
                | RPL_YOURESERVICE T.Text
                | RPL_TIME T.Text
                | RPL_USERSSTART T.Text
                | RPL_USERS T.Text
                | RPL_ENDOFUSERS
                | RPL_NOUSERS
                | RPL_TRACELINK T.Text
                | RPL_TRACECONNECTING T.Text
                | RPL_TRACEHANDSHAKE T.Text
                | RPL_TRACEUNKNOWN T.Text
                | RPL_TRACEOPERATOR T.Text
                | RPL_TRACEUSER T.Text
                | RPL_TRACESERVICE T.Text
                | RPL_TRACENEWTYPE T.Text
                | RPL_TRACECLASS T.Text
                | RPL_TRACELOG T.Text
                | RPL_TRACEEND T.Text
                | RPL_STATSLINKINFO T.Text
                | RPL_STATSCOMMANDS T.Text
                | RPL_ENDOFSTATS
                | RPL_STATSUPTIME T.Text
                | RPL_STATSOLINE T.Text
                | RPL_UMODEIS T.Text
                | RPL_SERVLIST T.Text
                | RPL_SERVLISTEND T.Text
                | RPL_LUSERCLIENT T.Text
                | RPL_LUSEROP T.Text
                | RPL_LUSERUNKNOWN T.Text
                | RPL_LUSERCHANNELS T.Text
                | RPL_LUSERME T.Text
                | RPL_ADMINME T.Text
                | RPL_ADMINLOC1 T.Text
                | RPL_ADMINLOC2 T.Text
                | RPL_ADMINEMAIL T.Text
                | RPL_TRYAGAIN T.Text

                | ERR_NOSUCHNICK T.Text
                | ERR_NOSUCHSERVER T.Text
                | ERR_NOSUCHCHANNEL T.Text
                | ERR_CANNOTSENDTOCHAN T.Text
                | ERR_TOOMANYCHANNELS T.Text
                | ERR_WASNOSUCHNICK T.Text
                | ERR_TOOMANYTARGETS T.Text
                | ERR_NOSUCHSERVICE T.Text
                | ERR_NOORIGIN
                | ERR_NORECIPIENT T.Text
                | ERR_NOTEXTTOSEND
                | ERR_NOTOPLEVEL T.Text
                | ERR_WILDTOPLEVEL T.Text
                | ERR_BADMASK T.Text
                | ERR_UNKNOWNCOMMAND T.Text
                | ERR_NOMOTD
                | ERR_NOADMININFO T.Text
                | ERR_FILEERROR T.Text
                | ERR_NONICKNAMEGIVEN
                | ERR_ERRONEUSNICKNAME T.Text
                | ERR_NICKNAMEINUSE T.Text
                | ERR_NICKCOLLISION T.Text
                | ERR_UNAVAILRESOURCE T.Text
                | ERR_USERNOTINCHANNEL T.Text
                | ERR_NOTONCHANNEL T.Text
                | ERR_USERONCHANNEL T.Text
                | ERR_NOLOGIN T.Text
                | ERR_SUMMONDISABLED
                | ERR_USERSDISABLED
                | ERR_NOTREGISTERES
                | ERR_NEEDMOREPARAMS T.Text
                | ERR_ALREADYREGISTERED
                | ERR_NOPERMFORHOST
                | ERR_PASSWDMISMATCH
                | ERR_YOUREBANNEDCREEP
                | ERR_YOUWILLBEBANNED
                | ERR_KEYSET T.Text
                | ERR_CHANISFULL T.Text
                | ERR_UNKNOWNMODE T.Text
                | ERR_INVITEONLYCHAN T.Text
                | ERR_BANNEDFROMCHAN T.Text
                | ERR_BADCHANNELKEY T.Text
                | ERR_BADCHANMASK T.Text
                | ERR_NOCHANMODES T.Text
                | ERR_BANLISTFULL T.Text
                | ERR_NOPRIVILEGES
                | ERR_CHANOPRIVSNEEDED T.Text
                | ERR_CANTKILLSERVER
                | ERR_RESTRICTED
                | ERR_UNIQOPPRIVSNEEDED
                | ERR_NOOPERHOST
                | ERR_UMODEUNKNOWNFLUG
                | ERR_USERSDONTMATCH
    deriving (Show)
