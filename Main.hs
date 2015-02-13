import System.Environment
import Control.Exception (bracket)
import Data.Maybe
import qualified Scripting.Lua as Lua
import Numeric
import Control.Monad.State
import Data.List

data Point a = Point a a a deriving (Show, Eq)

data Vector a = Vector a a a deriving (Show, Eq)

plus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector x0 y0 z0) `plus` (Vector x1 y1 z1) = Vector (x0+x1) (y0+y1) (z0+z1)

mult :: (Num a) => Vector a -> a -> Vector a
(Vector x y z) `mult` a = Vector (x*a) (y*a) (z*a)

-- todo Need to remove unnecessary digits from end.
r6 :: (RealFloat a) => a -> String
r6 n = showFFloat (Just 6) n ""

data Mode = Mill | Lathe deriving (Enum, Show)
data Motion = Move | Cut deriving (Enum, Show)

data Turtle = Turtle { mode :: Mode
                     , position :: Point Double
                     , a :: Double
                     , b :: Double
                     , motion :: Maybe Motion
                     , last_position :: Point Double
                     , feed :: Double
                     } deriving (Show)
newTurtle :: Mode -> Turtle
newTurtle m = Turtle { mode = m
                  , position = Point 0 0 0
                  , a = 0
                  , b = 0
                  , motion = Nothing
                  , last_position = Point 0 0 0
                  , feed = 0 }

data Word = F Double
          | G Double
          | X Double
          | Y Double
          | Z Double
    deriving (Eq, Read, Show)

type Block = [Word]

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
        otherwise -> usage

generatePosition, generateMove, generateCut :: Point Double -> Mode -> Block
generatePosition (Point x y z) Mill = [X x, Y y, Z z]
generatePosition (Point x y z) Lathe = [X x, Z y]
generateMove pos m = (G 0) : generatePosition pos m
generateCut pos m = (G 1) : generatePosition pos m

generateMotion :: Point Double -> Mode -> Motion -> Block
generateMotion pos m Move = generateMove pos m
generateMotion pos m Cut = generateCut pos m

turtleState :: Motion -> Point Double -> Double -> Turtle -> Turtle
turtleState m pos f t = t { position = pos
                          , motion = Just m
                          , feed = f }

-- Motion commands are stateful
-- let updatePos t pos = t { position = pos }
-- todo update state
move_to :: Point Double -> State Turtle Block
move_to pos@(Point x y z) = state $ \t -> 
    let curPos = generatePosition (position t) (mode t)
        rawBlock = generateMotion pos (mode t) Move
        block = rawBlock \\ curPos
        newState = turtleState Move pos 0
    in
        if pos /= position t then (block, newState t)
        else ([],t)

pow :: Double -> Double -> IO Double
pow d1 d2 = return $ d1 ** d2

