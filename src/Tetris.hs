{-# LANGUAGE FlexibleContexts #-}
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
  , block, board, level, nextShape, score, shape, linesCleared, progression
  -- Constants
  , boardHeight, boardWidth, relCells
  ) where

import Prelude hiding (Left, Right)
import Control.Applicative ((<|>))
import Control.Monad (forM_, mfilter, when, (<=<))

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.State.Class (MonadState, gets, put)
import Control.Monad.Trans.State (evalStateT)
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
  , _linesCleared :: Int
  , _score        :: Int
  , _board        :: Board
  , _progression  :: Bool
  } deriving (Eq)
makeLenses ''Game

evalTetris :: Tetris a -> Game -> a
evalTetris m = runIdentity . evalStateT m

type Tetris a = forall m. MonadState Game m => m a

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
initGame :: Int -> Bool -> IO Game  -- Updated signature
initGame lvl prog = do
  (s1, bag1) <- bagFourTetriminoEach mempty
  (s2, bag2) <- bagFourTetriminoEach bag1
  pure $ Game
    { _level        = lvl
    , _block        = initBlock s1
    , _nextShape    = s2
    , _nextShapeBag = bag2
    , _score        = 0
    , _linesCleared = 0
    , _board        = mempty
    , _progression  = prog  -- Added prog parameter
    }

-- | Increment level
nextLevel :: (MonadIO m, MonadState Game m) => m ()
nextLevel = do
  level %= (+ 1)

isGameOver :: Game -> Bool
isGameOver g = blockStopped g && g ^. (block . origin) == startOrigin

-- | The main game execution, this is executed at each discrete time step
timeStep :: (MonadIO m, MonadState Game m) => m ()
timeStep = do
  gets blockStopped >>= \case
    False -> gravitate
    True -> do
      freezeBlock
      clearFullRows >>= updateScore
      prog <- use progression
      when prog $ do
        levelFinished >>= \case
          True -> nextLevel
          False -> pure ()
      nextBlock

-- | Gravitate current block, i.e. shift down
gravitate :: MonadState Game m => m ()
gravitate = shift Down

-- | If necessary: clear full rows and return the count
clearFullRows :: MonadState Game m => m Int
clearFullRows = do
  brd <- use board
  let rowSize r = length $ M.filterWithKey (\(V2 _ y) _ -> r == y) brd
      fullRows = filter (\r -> boardWidth == rowSize r) [1 .. boardHeight]
  -- Clear cells in full rows
  modifying board $ M.filterWithKey $ \(V2 _ y) _ -> y `notElem` fullRows
  -- Shift cells above full rows
  modifying board $ M.mapKeysMonotonic $ over _y $ \y ->
    y - length (filter (< y) fullRows)
  let clearedLines = length fullRows
  linesCleared %= (+ clearedLines)
  pure clearedLines

-- | This updates game points with respect to the provided number of cleared
-- lines.
--
-- See https://tetris.fandom.com/wiki/Scoring
updateScore :: (MonadState Game m, MonadIO m) => Int -> m ()
updateScore 0 = pure ()
updateScore lines = do
  lvl <- use level
  let newPoints = (lvl + 1) * points lines
  score %= (+ newPoints)
  where
    -- Translate row line clears to points
    points 0 = 0
    points 1 = 40
    points 2 = 100
    points 3 = 300
    points _ = 1200

-- | Using the fixed-goal system described here: https://tetris.wiki/Marathon
levelFinished :: (MonadState Game m, MonadIO m) => m Bool
levelFinished = do
  prog <- use progression
  if not prog
    then pure False
    else do
      lvl <- use level
      lc <- use linesCleared
      pure $ lvl < 15 && lc >= 10 * (lvl + 1)

-- | Handle counterclockwise block rotation (if possible)
-- Allows wallkicks: http://tetris.wikia.com/wiki/TGM_rotation
rotate :: MonadState Game m => m ()
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

hardDrop :: MonadState Game m => m ()
hardDrop = hardDroppedBlock >>= assign block

hardDroppedBlock :: MonadState Game m => m Block
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
freezeBlock :: MonadState Game m => m ()
freezeBlock = do
  blk <- use block
  modifying board $ M.union $ M.fromList [ (c, _shape blk) | c <- coords blk ]

-- | Replace block with next block
nextBlock :: (MonadIO m, MonadState Game m) => m ()
nextBlock = do
  bag <- use nextShapeBag
  (t, ts) <- liftIO $ bagFourTetriminoEach bag
  use nextShape >>= \s -> block .= initBlock s
  nextShape .= t
  nextShapeBag .= ts

-- | Try to shift current block; if shifting not possible, leave block where it is
shift :: MonadState Game m => Direction -> m ()
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
    case Seq.splitAt randomPosition xs of
      (left, y :<| ys) ->  fmap (y <|) (shuffle $ left >< ys)
      _ -> error "impossible"

v2 :: (a, a) -> V2 a
v2 (x, y) = V2 x y
