import Control.Monad
import Data.Maybe
import System.Environment
import Irc

main = do
    args <- getArgs
    let nick : name : server : channels = args
    let config = Config server nick name channels
    (sendMessage, input) <- connect(config)
    mapM_ ((=<<) sendMessage) $ catMaybes (map roll input)

echo :: Message -> Maybe (IO Command)
echo (PrivateMessage nick text) = Just $ return (PrivMsg nick text)
echo (ChannelMessage channel nick text) = Just $ return (PrivMsg channel (nick ++ ": " ++ text))
echo _ = Nothing
