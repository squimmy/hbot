import Network
import System.IO
import Control.Monad
import Data.Maybe
import System.Environment
import Irc

main = do
    args <- getArgs
    let nick : name : server : channels = args
    let config = Config server nick name channels
    (sendMessage, input) <- connect(config)
    mapM_ sendMessage (mapMaybe echo input)

echo :: Message -> Maybe Command
echo (PrivateMessage nick text) = Just (PrivMsg nick text)
echo (ChannelMessage channel nick text) = Just (PrivMsg channel (nick ++ ": " ++ text))
echo _ = Nothing
