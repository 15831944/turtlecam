module Turtle
( Mode (..)
, newTurtle
, move_to
, Word (..)
, Block
) where
import Vector
import Data.Maybe
import Numeric
import Data.List
import Control.Monad.State

-- todo Need to remove unnecessary digits from end.
r6 :: (RealFloat a) => a -> String
r6 n = showFFloat (Just 6) n ""


data Mode = Mill | Lathe deriving (Enum, Show)
data Motion = Move | Cut deriving (Enum, Show)

data Turtle = Turtle { mode :: Mode
                     , position :: Point
                     , a :: Double
                     , b :: Double
                     , motion :: Maybe Motion
                     , last_position :: Point
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

generatePosition, generateMove, generateCut :: Point -> Mode -> Block
generatePosition (Point x y z) Mill = [X x, Y y, Z z]
generatePosition (Point x y z) Lathe = [X x, Z y]
generateMove pos m = (G 0) : generatePosition pos m
generateCut pos m = (G 1) : generatePosition pos m

generateMotion :: Point -> Mode -> Motion -> Block
generateMotion pos m Move = generateMove pos m
generateMotion pos m Cut = generateCut pos m

turtleState :: Motion -> Point -> Double -> Turtle -> Turtle
turtleState m pos f t = let last_pos = position t 
                        in
                            t { position = pos
                              , motion = Just m
                              , feed = f 
                              , last_position = last_pos }

-- Motion commands are stateful
-- let updatePos t pos = t { position = pos }
-- todo update state
move_to :: (Double, Double, Double) -> State Turtle Block
move_to (x, y, z) = state $ \t -> 
    let pos = Point x y z
        curPos = generatePosition (position t) (mode t)
        rawBlock = generateMotion pos (mode t) Move
        block = rawBlock \\ curPos
        newState = turtleState Move pos 0
    in
        if pos /= position t then (block, newState t)
        else ([],t)

