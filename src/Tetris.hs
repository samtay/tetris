{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Tetris
  (
  -- Game state modifiers
    initGame
  , timeStep
  , shift
  , rotate
  , hardDrop
  -- Game state handlers
  , execTetris
  , evalTetris
  -- Game state queries
  , isGameOver
  , hardDroppedBlock
  , coords
  -- Types
  , Block(..)
  , Coord
  , Direction(..)
  , Game(..)
  , Tetrimino(..)
  , Tetris
  -- Lenses
  , block, board, level, nextShape, score, shape
  -- Constants
  , boardHeight, boardWidth, relCells
  ) where

import Prelude hiding (Left, Right)
import Control.Applicative ((<|>))
import Control.Monad (forM_, mfilter, when, (<=<))
import Control.Monad.IO.Class (MonadIO(..), liftIO)

import Control.Monad.Trans.State (StateT(..), gets, evalStateT, execStateT)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq
import Control.Lens hiding (Empty)
import Linear.V2 (V2(..), _y)
import qualified Linear.V2 as LV
import System.Random (getStdRandom, randomR)
-- Types and instances

-- | Tetris shape types
data Tetrimino = I | O | T | S | Z | J | L
  deriving (Eq, Show, Enum)

-- | Coordinates
type Coord = V2 Int

-- | Tetris shape in location context
data Block = Block
  { _shape  :: Tetrimino -- ^ block type
  , _origin :: Coord -- ^ origin
  , _extra  :: [Coord] -- ^ extraneous cells
  } deriving (Eq, Show)

makeLenses ''Block

data Direction = Left | Right | Down
  deriving (Eq, Show)

-- | Board
--
-- If coordinate not present in map, yet in bounds, then it is empty,
-- otherwise its value is the type of tetrimino occupying it.
type Board = Map Coord Tetrimino

-- | Game state
data Game = Game
  { _level        :: Int
  , _block        :: Block
  , _nextShape    :: Tetrimino
  , _nextShapeBag :: Seq.Seq Tetrimino
  , _rowClears    :: Seq.Seq Int
  , _score        :: Int
  , _board        :: Board
  } deriving (Eq, Show)
makeLenses ''Game

type TetrisT = StateT Game
type Tetris a = forall m. (Monad m) => TetrisT m a

evalTetris :: Tetris a -> Game -> a
evalTetris m = runIdentity . evalStateT m

execTetris :: Tetris a -> Game -> Game
execTetris m = runIdentity . execStateT m

-- Translate class for direct translations, without concern for boundaries
-- 'shift' concerns safe translations with boundaries
class Translatable s where
  translate :: Direction -> s -> s
  translate = translateBy 1
  translateBy :: Int -> Direction -> s -> s

instance Translatable Coord where
  translateBy n Left (V2 x y)  = V2 (x-n) y
  translateBy n Right (V2 x y) = V2 (x+n) y
  translateBy n Down (V2 x y)  = V2 x (y-n)

instance Translatable Block where
  translateBy n d b =
    b & origin %~ translateBy n d
      & extra  %~ fmap (translateBy n d)

-- Low level functions on blocks and coordinates

initBlock :: Tetrimino -> Block
initBlock t = Block t startOrigin . fmap (+ startOrigin) . relCells $ t

relCells :: Tetrimino -> [Coord]
relCells I = map v2 [(-2, 0), (-1, 0), (1, 0)]
relCells O = map v2 [(-1, 0), (-1, -1), (0, -1)]
relCells S = map v2 [(-1, -1), (0, -1), (1, 0)]
relCells Z = map v2 [(-1, 0), (0, -1), (1, -1)]
relCells L = map v2 [(-1, -1), (-1, 0), (1, 0)]
relCells J = map v2 [(-1, 0), (1, 0), (1, -1)]
relCells T = map v2 [(-1, 0), (0, -1), (1, 0)]

-- | Visible, active board size
boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

-- | Starting block origin
startOrigin :: Coord
startOrigin = V2 6 22

-- | Rotate block counter clockwise about origin
-- *Note*: Strict unsafe rotation not respecting boundaries
-- Safety can only be assured within Game context
rotateRaw :: Block -> Block
rotateRaw b@(Block s o@(V2 xo yo) cs)
  | -- O doesn't need rotation
    s == O                             = b
  | -- I only has two orientations
    s == I && V2 xo (yo + 1) `elem` cs = rotateWith clockwise
  | otherwise                          = rotateWith counterclockwise
 where
  clockwise        = (+ o) . cwperp . subtract o
  counterclockwise = (+ o) . LV.perp . subtract o
  rotateWith dir = b & extra %~ fmap dir
  cwperp (V2 x y) = V2 y (-x)

-- | Get coordinates of entire block
coords :: Block -> [Coord]
coords b = b ^. origin : b ^. extra

-- Higher level functions on game and board

-- | Facilitates cycling through at least 4 occurences of each shape
-- before next bag (random permutation of 4*each tetrimino) is created. If input is empty,
-- generates new bag, otherwise just unshifts the first value and returns pair.
bagFourTetriminoEach :: Seq.Seq Tetrimino -> IO (Tetrimino, Seq.Seq Tetrimino)
bagFourTetriminoEach (t :<| ts) = pure (t, ts)
bagFourTetriminoEach Empty =
  bagFourTetriminoEach <=< shuffle . Seq.fromList . take 28 $ cycle [I ..]

-- | Initialize a game with a given level
initGame :: Int -> IO Game
initGame lvl = do
  (s1, bag1) <- bagFourTetriminoEach mempty
  (s2, bag2) <- bagFourTetriminoEach bag1
  pure $ Game
    { _level        = lvl
    , _block        = initBlock s1
    , _nextShape    = s2
    , _nextShapeBag = bag2
    , _score        = 0
    , _rowClears    = mempty
    , _board        = mempty
    }

isGameOver :: Game -> Bool
isGameOver g = blockStopped g && g ^. (block . origin) == startOrigin

-- | The main game execution, this is executed at each discrete time step
timeStep :: MonadIO m => TetrisT m ()
timeStep = do
  gets blockStopped >>= \case
    False -> gravitate
    True -> do
      freezeBlock
      clearFullRows >>= addToRowClears
      updateScore
      nextBlock

-- | Gravitate current block, i.e. shift down
gravitate :: Tetris ()
gravitate = shift Down

-- | If necessary: clear full rows and return the count
clearFullRows :: Tetris Int
clearFullRows = do
  brd <- use board
  let rowSize r = length $ M.filterWithKey (\(V2 _ y) _ -> r == y) brd
      fullRows = filter (\r -> boardWidth == rowSize r) [1 .. boardHeight]
  -- Clear cells in full rows
  modifying board $ M.filterWithKey $ \(V2 _ y) _ -> y `notElem` fullRows
  -- Shift cells above full rows
  modifying board $ M.mapKeysMonotonic $ over _y $ \y ->
    y - length (filter (< y) fullRows)
  return $ length fullRows

-- | Empties row on 0, otherwise appends value (just keeps consecutive information)
addToRowClears :: Int -> Tetris ()
addToRowClears 0 = rowClears .= mempty
addToRowClears n = rowClears %= (|> n)

-- | This updates game points with respect to the current
-- _rowClears value (thus should only be used ONCE per step)
--
-- Note I'm keeping rowClears as a sequence in case I want to award
-- more points for back to back clears, right now the scoring is more simple,
-- but you do get more points for more rows cleared at once.
updateScore :: Tetris ()
updateScore = do
  multiplier <- (1 +) <$> use level
  clears <- latestOrZero <$> use rowClears
  let newPoints = multiplier * points clears
  score %= (+ newPoints)
  where
    -- Translate row clears to points
    points 0 = 0
    points 1 = 40
    points 2 = 100
    points 3 = 300
    points _ = 800
    -- | Get last value of sequence or 0 if empty
    latestOrZero :: Seq.Seq Int -> Int
    latestOrZero Empty     = 0
    latestOrZero (_ :|> n) = n

-- | Handle counterclockwise block rotation (if possible)
-- Allows wallkicks: http://tetris.wikia.com/wiki/TGM_rotation
rotate :: Tetris ()
rotate = do
  blk <- use block
  brd <- use board
  let mblk = foldr (<|>) Nothing
        $   mfilter (isValidBlockPosition brd)
        .   pure
        .   ($ blk)
        <$> [ rotateRaw
            , rotateRaw . translate Left
            , rotateRaw . translate Right
            ]
  forM_ mblk $ assign block

blockStopped :: Game -> Bool
blockStopped g = isStopped (g ^. board) (g ^. block)

-- | Check if a block on a board is stopped from further gravitation
isStopped :: Board -> Block -> Bool
isStopped brd = any stopped . coords
 where
  stopped = (||) <$> atBottom <*> (`M.member` brd) . translate Down
  atBottom = (== 1) . view _y

hardDrop :: Tetris ()
hardDrop = hardDroppedBlock >>= assign block

hardDroppedBlock :: Tetris Block
hardDroppedBlock = do
  boardCoords <- M.keys <$> use board
  blockCoords <- coords <$> use block
  let diffs =
        [ y - yo
        | (V2 xo yo) <- boardCoords
        , (V2 x  y ) <- blockCoords
        , xo == x
        , yo < y
        ]
      minY = minimum $ view _y <$> blockCoords
      dist = minimum $ subtract 1 <$> (minY : diffs)
  translateBy dist Down <$> use block

-- | Freeze current block
freezeBlock :: Tetris ()
freezeBlock = do
  blk <- use block
  modifying board $ M.union $ M.fromList [ (c, _shape blk) | c <- coords blk ]

-- | Replace block with next block
nextBlock :: MonadIO m => TetrisT m ()
nextBlock = do
  bag <- use nextShapeBag
  (t, ts) <- liftIO $ bagFourTetriminoEach bag
  use nextShape >>= \s -> block .= initBlock s
  nextShape .= t
  nextShapeBag .= ts

-- | Try to shift current block; if shifting not possible, leave block where it is
shift :: Direction -> Tetris ()
shift dir = do
  brd <- use board
  blk <- use block
  let candidate = translate dir blk
  when (isValidBlockPosition brd candidate) $
    block .= candidate

-- | Check if coordinate is already occupied or free in board
isFree :: Board -> Coord -> Bool
isFree = flip M.notMember

-- | Check if coordinate is in or out of bounds
isInBounds :: Coord -> Bool
isInBounds (V2 x y) = 1 <= x && x <= boardWidth && 1 <= y

-- | Checks if block's potential new location is valid
isValidBlockPosition :: Board -> Block -> Bool
isValidBlockPosition brd = all validCoord . coords
  where validCoord = (&&) <$> isFree brd <*> isInBounds

-- General utilities

-- | Shuffle a sequence (random permutation)
shuffle :: Seq.Seq a -> IO (Seq.Seq a)
shuffle xs
  | null xs = mempty
  | otherwise = do
    randomPosition <- getStdRandom (randomR (0, length xs - 1))
    let (left, y :<| ys) = Seq.splitAt randomPosition xs
    fmap (y <|) (shuffle $ left >< ys)

v2 :: (a, a) -> V2 a
v2 (x, y) = V2 x y
