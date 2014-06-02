import Network
import System.IO
import Control.Monad
import Data.Maybe
import System.Environment
import Irc

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
