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
                (code, state) = runState (move_to (10, 0, 0)) (newTurtle Mill)
              in
                print code
        [file] -> doFile file
        otherwise -> usage


pow :: Double -> Double -> IO Double
pow d1 d2 = return $ d1 ** d2

