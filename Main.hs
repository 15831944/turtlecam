import System.Environment
import Control.Exception (bracket)
import Data.Maybe
import qualified Scripting.Lua as Lua

data Mode = Mill | Lathe deriving (Enum, Show)
data Motion = Move | Cut deriving (Enum, Show)
data Vector = Vector deriving (Show)

data Turtle = Turtle { position :: Vector
                     , a :: Double
                     , b :: Double
                     , motion :: Maybe Motion
                     , last_position :: Vector
                     , f :: Double
                     } deriving (Show)

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
        [] -> usage
        [file] -> doFile file

pow :: Double -> Double -> IO Double
pow d1 d2 = return $ d1 ** d2
