import System.Environment
import Control.Exception (bracket)
import qualified Scripting.Lua as Lua
import Turtle
import Control.Monad.State
import Vector


usage :: IO ()
usage = putStrLn "turtlecam [command]"

doFile :: String -> IO ()
doFile file = do
    bracket (Lua.newstate) (Lua.close) (\l -> do
        Lua.registerhsfunction l "pow" pow
        Lua.loadfile l file
        Lua.call l 0 0)

main :: IO ()
main = do
    args <- getArgs
    case args of
--        [] -> usage
        [] -> let 
                (code, state) = runState millSquare (newTurtle Mill)
              in do
                print code
                print state
        [file] -> doFile file
        otherwise -> usage


millSquare = do
    a <- move_to(10, 0, 0)
    b <- move_to(10, 10, 0)
    c <- move_to(0, 10, 0)
    d <- move_to(0, 0, 0)
    return a-- ++ b ++ c ++ d


pow :: Double -> Double -> IO Double
pow d1 d2 = return $ d1 ** d2

