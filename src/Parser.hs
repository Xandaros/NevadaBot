{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Parser ( messageParser
              , parse -- re-export from Text.ParserCombinators.Parsec
              ) where

import           Data.List
import qualified Data.Text as T
import           Data.Maybe
import           Debug.Trace
import           Text.ParserCombinators.Parsec

import           Types hiding (nickname, prefix, command)

messageParser :: Parser IRCMessage
messageParser = (IRCMessage <$> prefix) <*> command

prefix :: Parser (Maybe IRCPrefix)
prefix = optionMaybe $ do
    char ':'
    try userPrefix <|> serverPrefix

serverPrefix :: Parser IRCPrefix
serverPrefix = do
    host <- hostname
    char ' '
    return $ IRCPrefix host Nothing Nothing

userPrefix :: Parser IRCPrefix
userPrefix = do
    nick <- nickname
    user <- optionMaybe $ do
        char '!'
        user
    host <- optionMaybe $ do
        char '@'
        hostname
    char ' '
    return $ IRCPrefix nick user host

-- TODO: optimise
nickname :: Parser T.Text
nickname = T.pack <$> do
    firstChar  <- letter <|> special
    otherChars <- many $ letter <|> digit <|> special
    return $ firstChar:otherChars

special :: Parser Char
special = oneOf "[]\\`_^{|}"

hostname :: Parser T.Text
hostname = T.pack <$> do
    first <- shortname
    rest  <- many $ do
        char '.' 
        ('.':) <$> shortname
    return $ first ++ concat rest

shortname :: Parser String
shortname = do
    first  <- letter <|> digit
    middle <- many $ letter <|> digit <|> char '-'
    end    <- many $ letter <|> digit
    return $ first:middle ++ end

command :: Parser IRCCommand
command = do
    command  <- many1 digit <|> many1 letter
    args     <- many (try $ char ' ' >> middle)
    trailing <- option "" $ do
        char ' '
        char ':'
        many (char ' ' <|> char ':' <|> nospclrfcl)
    let argsf  = take 14 args ++ [unwords . drop 14 $ args]
    let argsf' = argsf ++ [trailing]
    return $ UNKNOWNMSG (T.pack command) (T.pack <$> filter (/="") argsf')

nospclrfcl :: Parser Char
nospclrfcl = noneOf "\0 \r\n:"

middle :: Parser String
middle = do
    first <- nospclrfcl
    rest  <- many $ char ':' <|> nospclrfcl
    return $ first:rest

user :: Parser T.Text
user = T.pack <$> many (noneOf "\0\r\n @")
