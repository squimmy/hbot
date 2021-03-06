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
import Data.Maybe
import Data.String.Utils
import Network
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
    handle <- conn (server config)
    let send = flip sendCommand handle
    line <- liftM lines $ hGetContents handle
    send $ Nick (nick config)
    send $ User (nick config) (realname config)
    mapM_ (send . Join) (channels config)
    hFlush handle
    let (pings, other) = partition isPing (map (parseMessage (nick config)) line)
    _ <- forkIO (mapM_ send (catMaybes $ map reply pings))
    return (send, other)
    where
        conn serverAddress = withSocketsDo $ do
            connectTo serverAddress $ PortNumber 6667
        isPing (Ping _) = True
        isPing _ = False
        reply (Ping s) = Just $ Pong s
        reply _ = Nothing

sendCommand :: Command -> Handle -> IO ()
sendCommand x = case x of
    (User un rn) -> sendMessage ("USER " ++ un ++ " 8 * :" ++ rn)
    (Nick n) -> sendMessage ("NICK " ++ n)
    (Join c) -> sendMessage ("JOIN " ++ c)
    (Pong s) -> sendMessage ("PONG " ++ s)
    (PrivMsg r t) -> sendMessage ("PRIVMSG " ++ r ++ " :" ++ t)

sendMessage :: String -> Handle -> IO ()
sendMessage = flip hPutStr . (flip  (++) "\r\n") . (take 510)


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
        pRec xs acc       = pRec (lstrip r) (param:acc) where (param, r) = break (==' ') xs

