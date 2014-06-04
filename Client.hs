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
    mapM_ print input

