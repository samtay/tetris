{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Tetris
    (
    ) where

import qualified Data.Map as M
import Lens.Micro
import Lens.Micro.TH
import Prelude hiding (Left, Right)

-- | Tetris shape types
data Tetrimino = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum)

-- | Coordinates
type Coord = (Int, Int)

-- | Tetris shape in coordinate context
data Block = Block
  { _shape  :: Tetrimino -- ^ block type
  , _origin :: Coord -- ^ origin (absolute)
  , _extra  :: [Coord] -- ^ extraneous cells (relative)
  } deriving (Eq, Show)

makeLenses ''Block

data Direction = Left | Right | Down
  deriving (Eq, Show)

-- | Cell state within a tetris board
data Cell = Filled Tetrimino | Empty
  deriving (Eq, Show)

-- | Board of cells
type Board = M.Map Coord Cell

-- | Game state
data Game = Game
  { _speed :: Int
  , _currBlock :: Block
  , _nextShape :: Tetrimino
  , _score :: Int
  , _board :: Board
  } deriving (Eq, Show)


-- Translate class for direct translations, without concern for boundaries
-- Shiftable concerns safe translations with boundaries
class Translatable s where
  translate :: Direction -> s -> s

instance Translatable Coord where
  translate Left (x, y) = (x-1, y)
  translate Right (x, y) = (x+1, y)
  translate Down (x,y) = (x, y-1)

instance Translatable Block where
  translate d b = b & origin %~ translate d

initI, initO, initS, initZ, initL, initJ, initT :: Block
initI = Block I (0,0) [(-2,0), (-1,0), (1,0)]
initO = Block O (0,0) [(-1,0), (-1,-1), (0,-1)]
initS = Block S (0,0) [(-1,-1), (0,-1), (1,0)]
initZ = Block Z (0,0) [(-1,0), (0,-1), (1,-1)]
initL = Block L (0,0) [(-1,-1), (-1,0), (1,0)]
initJ = Block J (0,0) [(-1,0), (1,0), (1,-1)]
initT = Block T (0,0) [(-1,0), (0,-1), (1,0)]

-- | Rotate block counter clockwise about origin
-- *Note*: Strict unsafe rotation not respecting boundaries
-- Safety can only be assured within Game context
rotate' :: Block -> Block
rotate' b@(Block s o cs)
  | s == O = b -- O doesn't need rotation
  | s == I && (-2,0) `elem` cs = rotateBy counterclockwise
  | s == I = rotateBy clockwise
rotate' = (& extra %~ fmap counterclockwise)

rotateBy :: (Coord -> Coord) -> Block -> Block
rotateBy dir = (& extra %~ fmap dir)

clockwise :: Coord -> Coord
clockwise (x, y) = (y, -x)

counterclockwise :: Coord -> Coord
counterclockwise (x, y) = (-y, x)

-- | Get absolute coordinates of extraneous block cells
absExtra :: Block -> [Coord]
absExtra (Block _ (xo,yo) cs) = fmap (\(x,y) -> (x+xo, y+yo)) cs

-- | Get absolute coordinates of all block cells
absAll :: Block -> [Coord]
absAll b = b ^. origin : absExtra b

-- | TODO wallkicks http://tetris.wikia.com/wiki/TGM_rotation
