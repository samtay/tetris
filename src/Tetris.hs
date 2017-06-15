{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Tetris where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (ViewL(..), (<|), (><))
import qualified Data.Sequence as Seq
import Lens.Micro
import Lens.Micro.TH
import System.Random (getStdRandom, randomR)

import Prelude hiding (Left, Right)

-- Types and instances

-- | Tetris shape types
data Tetrimino = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum)

-- | Coordinates
type Coord = (Int, Int)

-- | Tetris shape in coordinate context
data Block = Block
  { _shape  :: Tetrimino -- ^ block type
  , _origin :: Coord -- ^ origin
  , _extra  :: [Coord] -- ^ extraneous cells
  } deriving (Eq, Show)

makeLenses ''Block

data Direction = Left | Right | Down
  deriving (Eq, Show)

-- | Cell state within a tetris board
data Cell = Filled Tetrimino | Empty
  deriving (Eq, Show)

-- | Board of cells
type Board = Map Coord Cell

-- | Game state
data Game = Game
  { _level :: Int
  , _currBlock :: Block
  , _nextShape :: Tetrimino
  , _nextShapeBag :: Seq.Seq Tetrimino
  , _score :: Int
  , _board :: Board
  } deriving (Eq, Show)

makeLenses ''Game


-- Translate class for direct translations, without concern for boundaries
-- Shiftable concerns safe translations with boundaries
class Translatable s where
  translate :: Direction -> s -> s

instance Translatable Coord where
  translate Left (x, y) = (x-1, y)
  translate Right (x, y) = (x+1, y)
  translate Down (x,y) = (x, y-1)

instance Translatable Block where
  translate d b =
    b & origin %~ translate d
      & extra %~ fmap (translate d)

-- Low level functions on blocks, cells, and coordinates

initBlock :: Tetrimino -> Block
initBlock I = Block I startOrigin [(-2,0), (-1,0), (1,0)]
initBlock O = Block O startOrigin [(-1,0), (-1,-1), (0,-1)]
initBlock S = Block S startOrigin [(-1,-1), (0,-1), (1,0)]
initBlock Z = Block Z startOrigin [(-1,0), (0,-1), (1,-1)]
initBlock L = Block L startOrigin [(-1,-1), (-1,0), (1,0)]
initBlock J = Block J startOrigin [(-1,0), (1,0), (1,-1)]
initBlock T = Block T startOrigin [(-1,0), (0,-1), (1,0)]

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

-- | Starting block origin cell
startOrigin :: Coord
startOrigin = (6, 21)

-- | Rotate block counter clockwise about origin
-- *Note*: Strict unsafe rotation not respecting boundaries
-- Safety can only be assured within Game context
rotate' :: Block -> Block
rotate' b@(Block s o@(xo,yo) cs)
  | s == O = b -- O doesn't need rotation
  | s == I && (xo,yo+1) `elem` cs = rotateWith clockwise b -- I only has two orientations
  | otherwise = rotateWith counterclockwise b
  where
    rotateWith :: (Coord -> Coord -> Coord) -> Block -> Block
    rotateWith dir b = b & extra %~ fmap (dir (b ^. origin))

    clockwise :: Coord -- ^ origin
              -> Coord -- ^ point to rotate around origin
              -> Coord
    clockwise (xo, yo) (x, y) = (xo + y - yo, xo + y - x)

    counterclockwise :: Coord -- ^ origin
                     -> Coord -- ^ point to rotate around origin
                     -> Coord
    counterclockwise (xo, yo) (x, y) = (xo + yo - y, x + yo - xo)

-- | Get coordinates of all block cells
occupiedCells :: Block -> [Coord]
occupiedCells b = b ^. origin : b ^. extra

-- Higher level functions on game and board

bagFourTetriminoEach :: IO (Seq.Seq Tetrimino)
bagFourTetriminoEach =
  shuffle $ Seq.cycleTaking 28 $ Seq.fromList [(I)..]

-- | Initialize a game with a given level
initGame :: Int ->  IO Game
initGame lvl = do
  initBag <- bagFourTetriminoEach
  let (fstShape :< fstBag) = Seq.viewl initBag
      (sndShape :< sndBag) = Seq.viewl fstBag
  return
    Game { _level = lvl
         , _currBlock = initBlock fstShape
         , _nextShape = sndShape
         , _nextShapeBag = sndBag
         , _score = 0
         , _board = mempty }

-- TODO check if mapKeysMonotonic works
clearFullRows :: Game -> Game
clearFullRows g = g & board %~ clearBoard
  where clearBoard           = M.mapKeys shiftRowsAbove . M.filterWithKey isInFullRow
        isInFullRow (_,y) _  = y `elem` fullRowIndices
        fullRowIndices       = filter isFullRow [1..boardHeight]
        isFullRow r          = boardWidth == (length . M.filterWithKey (inRow r) $ g ^. board)
        inRow r (_, y) _     = r == y
        shiftRowsAbove (x,y) =
          let offset = length . filter (< y) $ fullRowIndices
           in (x, y - offset)

-- TODO wallkicks http://tetris.wikia.com/wiki/TGM_rotation

shuffle :: Seq.Seq a -> IO (Seq.Seq a)
shuffle xs
  | null xs   = mempty
  | otherwise = do
      randomPosition <- getStdRandom (randomR (0, length xs - 1))
      let (left, right) = Seq.splitAt randomPosition xs
          (y :< ys)     = Seq.viewl right
      fmap (y <|) (shuffle $ left >< ys)
