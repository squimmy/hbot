module Irc
( Command(User, Nick, Join, Pong)
, Message(Msg)
, sendCommand
, parseMessage
) where

import System.IO

data Command
	= User String String
	| Nick String
	| Join String
	| Pong String


data Message = Msg (Maybe String) [String]

sendCommand (User username realname) = flip hPutStr ("USER " ++ username ++ " 8 * :" ++ realname ++ "\n")
sendCommand (Nick nick) = flip hPutStr ("NICK " ++ nick ++ "\n")
sendCommand (Join channel) = flip hPutStr ("JOIN " ++ channel ++ "\n")
sendCommand (Pong server) = flip hPutStr ("PONG " ++ server ++ "\n")

parseMessage :: String -> Maybe Message
parseMessage (' ':xs) = parseMessage $ ltrim xs
parseMessage (':':xs) = Just $ Msg (Just prefix) (pRec (ltrim content) []) where (prefix, content) = break (==' ') xs
parseMessage xs = Just $ Msg Nothing (pRec xs [])

pRec :: String -> [String] -> [String]
pRec [] acc       = reverse acc
pRec (':':xs) acc = reverse $ xs:acc
pRec xs acc       = pRec (ltrim rem) (param:acc) where (param, rem) = break (==' ') xs

ltrim = dropWhile (==' ')
