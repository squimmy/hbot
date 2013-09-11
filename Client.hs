import Network
import System.IO
import Control.Monad
import Data.Maybe
import System.Environment
import Irc

handleMessage :: String -> Handle -> IO ()
handleMessage message = case parseMessage message of
	Just (Msg _ ["PING", server]) -> sendCommand $ Pong server
	Just (Msg (Just prefix) content) -> (\ h -> putStrLn (prefix ++ show content))
	_ -> (\ h -> putStrLn message)



connect :: String -> IO Handle
connect server = withSocketsDo $ do
	connectTo server $ PortNumber 6667 


main = do
	args <- getArgs
	let nick : name : server : channels = args

	handle <- connect server
	line <- liftM lines $ hGetContents handle
	sendCommand(Nick nick) handle
	sendCommand(User nick name) handle
	mapM_ ((\ c -> sendCommand c handle) . Join) channels
	hFlush handle
	mapM_ (flip handleMessage handle) line
