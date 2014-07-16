import Control.Monad
import Data.Maybe
import Data.String.Utils as String
import Dice
import Irc
import Irc.Formatting
import System.Environment

main :: IO ()
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
    return (fmap (PrivMsg channel . (++) (nick ++ ": ") . formatResult) r)
roll _ = Nothing

formatResult :: ([Die], Int) -> String
formatResult (dice, total) = "[" ++ (String.join " " (map value dice)) ++ "]: " ++ (show total)
    where value (Die d v) | v == d    = format (Green, Silver) (" "++(show v)++" ")
                          | v == 1    = format (Red, Silver) (" "++(show v)++" ")
                          | otherwise = format (Black, Silver) (" "++(show v)++" ")

