import Control.Monad
import Data.Maybe
import System.Environment
import Irc
import Dice

main = do
    args <- getArgs
    let nick : name : server : channels = args
    let config = Config server nick name channels
    (sendMessage, input) <- connect(config)
    mapM_ ((=<<) sendMessage) $ catMaybes (map roll input)

roll :: Message -> Maybe (IO Command)
roll (PrivateMessage nick text) = do
    r <- evaluate text
    return (fmap (PrivMsg nick . show) r)
roll (ChannelMessage channel nick text) = do
    r <- evaluate text
    return (fmap (PrivMsg channel . (++) (nick ++ ": ") . show) r)
roll _ = Nothing
