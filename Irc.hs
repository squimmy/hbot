module Irc
( Command(User, Nick, Join, PrivMsg)
, Message(PrivateMessage, ChannelMessage, Other, ParseFailed)
, connect
, Config(Config)
, RawMessage(Msg)
, parseMessage
) where

import Control.Concurrent
import Control.Monad
import Data.List
import Data.String.Utils
import Network
import System.Environment
import System.IO

type Nick = String
type Channel = String
type Server = String
type SendCommand = Command -> IO ()

data Config = Config { server :: String
                     , nick :: String
                     , realname :: String
                     , channels :: [String]
                     }

data Command
    = User Nick String
    | Nick Nick
    | Join Channel
    | Pong Server
    | PrivMsg String String
    deriving (Show)

data Message
    = Ping Server
    | PrivateMessage Nick String
    | ChannelMessage Channel Nick String
    | Other String
    | ParseFailed
    deriving (Show)

data RawMessage = Msg (Maybe String) [String]

connect :: Config -> IO (SendCommand, [Message])
connect config = do
    handle <- connect (server config)
    let send = flip sendCommand handle
    line <- liftM lines $ hGetContents handle
    send $ Nick (nick config)
    send $ User (nick config) (realname config)
    mapM_ (send . Join) (channels config)
    hFlush handle
    let (pings, other) = partition isPing (map (parseMessage (nick config)) line)
    forkIO (mapM_ send (map reply pings))
    return (send, other)
    where
        connect server = withSocketsDo $ do
            connectTo server $ PortNumber 6667
        isPing (Ping _) = True
        isPing _ = False
        reply (Ping s) = Pong s

sendCommand (User username realname) = flip hPutStr ("USER " ++ username ++ " 8 * :" ++ realname ++ "\n")
sendCommand (Nick nick) = flip hPutStr ("NICK " ++ nick ++ "\n")
sendCommand (Join channel) = flip hPutStr ("JOIN " ++ channel ++ "\n")
sendCommand (Pong server) = flip hPutStr ("PONG " ++ server ++ "\n")
sendCommand (PrivMsg recipient text) = flip hPutStr ("PRIVMSG " ++ recipient ++ " :" ++ text ++ "\n")

parseMessage :: Nick -> String -> Message
parseMessage nick message = case pm message of
    Just (Msg _ ["PING", server]) -> Ping server
    Just (Msg (Just prefix) ["PRIVMSG", recipient, text]) | recipient == nick -> PrivateMessage (extractNick prefix) (rstrip text)
                                                          | otherwise -> ChannelMessage recipient (extractNick prefix) (rstrip text)
    Just (Msg (Just prefix) content) -> Other (prefix ++ show content)
    _ -> ParseFailed
    where
        pm :: String -> Maybe RawMessage
        pm (' ':xs) = pm $ lstrip xs
        pm (':':xs) = Just $ Msg (Just prefix) (pRec (lstrip content) []) where (prefix, content) = break (==' ') xs
        pm xs = Just $ Msg Nothing (pRec xs [])

        extractNick = takeWhile (/= '!')

        pRec :: String -> [String] -> [String]
        pRec [] acc       = reverse acc
        pRec (':':xs) acc = reverse $ xs:acc
        pRec xs acc       = pRec (lstrip rem) (param:acc) where (param, rem) = break (==' ') xs

