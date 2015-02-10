import System.Environment
import Control.Exception (bracket)
import Data.Maybe
import qualified Scripting.Lua as Lua

data Mode = Mill | Lathe deriving (Enum, Show)
data Motion = Move | Cut deriving (Enum, Show)

data Vector a = Vector a a a deriving (Show, Eq)

plus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x0 y0 z0) `plus` (Vector x1 y1 z1) = Vector (x0+x1) (y0+y1) (z0+z1)

mult :: (Num a) => Vector a -> a -> Vector a
(Vector x y z) `mult` a = Vector (x*a) (y*a) (z*a)


data Turtle = Turtle { position :: Vector Double
                     , a :: Double
                     , b :: Double
                     , motion :: Maybe Motion
                     , last_position :: Vector Double
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

